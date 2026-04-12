# Built-in Objects

*For contributors adding or modifying built-in objects, and for script authors looking up available APIs.*

## Executive Summary

- **Unconditional registration** — Standard built-ins (Console, Math, Object, Array, String, Number, RegExp, JSON, JSON5, JSONL, TOML, YAML, Symbol, Set, Map, Promise, Performance, Temporal, ArrayBuffer, SharedArrayBuffer, TypedArrays, Proxy, Reflect, Iterator, TextEncoder, TextDecoder, URL, URLSearchParams) are always registered
- **Flag-gated extras** — Only `ggTestAssertions`, `ggBenchmark`, and `ggFFI` use flag-gating for opt-in registration
- **Adding new built-ins** — See [Adding Built-in Types](adding-built-in-types.md) for the step-by-step recipe
- **Always-present globals** — `globalThis` and `Goccia` namespace are registered after all built-ins

GocciaScript provides a set of built-in global objects that mirror JavaScript's standard library. Each built-in is implemented as a Pascal unit and registered unconditionally by the engine. Only test assertions, benchmarks, and FFI use flag-gated registration.

## Registration System

Standard built-ins (Console, Math, Object, Array, Number, JSON, JSON5, JSONL, TOML, YAML, Symbol, Set, Map, Promise, Performance, Temporal, ArrayBuffer, Proxy, Reflect) are always registered unconditionally by the engine. There is no flag-gating for these — they are available in every execution context.

Only three built-ins use flag-gated registration via the `TGocciaGlobalBuiltins` enum:

```pascal
TGocciaGlobalBuiltin = (
  ggTestAssertions,   // describe, test, expect (testing only)
  ggBenchmark,        // suite, bench, runBenchmarks (benchmarking only)
  ggFFI              // Foreign Function Interface
);
```

The `TestRunner` adds `ggTestAssertions` to inject the test framework.
The `BenchmarkRunner` adds `ggBenchmark` to inject the benchmark framework.
FFI (`ggFFI`) is available but must be explicitly enabled.

## Adding a New Built-in

See [Adding a New Built-in Type](adding-built-in-types.md) for the complete step-by-step guide with code templates, GC considerations, and a checklist.

## Available Built-ins

### Console (`Goccia.Builtins.Console.pas`)

| Method | Description |
|--------|-------------|
| `console.log(...args)` | Output to stdout with space-separated values |
| `console.warn(...args)` | Output with "Warning:" prefix |
| `console.error(...args)` | Output with "Error:" prefix |
| `console.info(...args)` | Output with "Info:" prefix |
| `console.debug(...args)` | Output with "Debug:" prefix |
| `console.dir(obj)` | Display object properties |
| `console.assert(condition, ...args)` | Print if assertion fails |
| `console.count(label?)` | Increment and print named counter |
| `console.countReset(label?)` | Reset named counter |
| `console.time(label?)` | Start a named timer |
| `console.timeEnd(label?)` | Stop timer and print elapsed time |
| `console.timeLog(label?)` | Print elapsed time without stopping timer |
| `console.clear()` | No-op (placeholder for terminal clearing) |
| `console.group(label?)` | Increase indentation level |
| `console.groupEnd()` | Decrease indentation level |
| `console.trace(...args)` | Print with "Trace:" prefix |
| `console.table(data)` | Display data (formatted output) |

### Math (`Goccia.Builtins.Math.pas`)

**Constants:**

| Constant | Value |
|----------|-------|
| `Math.PI` | 3.14159265358979... |
| `Math.E` | 2.71828182845904... |
| `Math.LN2` | 0.693147... |
| `Math.LN10` | 2.302585... |
| `Math.LOG2E` | 1.442695... |
| `Math.LOG10E` | 0.434294... |
| `Math.SQRT2` | 1.414213... |
| `Math.SQRT1_2` | 0.707106... |

**Methods:**

| Method | Description |
|--------|-------------|
| `Math.abs(x)` | Absolute value |
| `Math.floor(x)` | Round down |
| `Math.ceil(x)` | Round up |
| `Math.round(x)` | Round to nearest |
| `Math.trunc(x)` | Truncate decimal |
| `Math.max(...args)` | Maximum value |
| `Math.min(...args)` | Minimum value |
| `Math.pow(base, exp)` | Exponentiation |
| `Math.sqrt(x)` | Square root |
| `Math.random()` | Random [0, 1) |
| `Math.sign(x)` | Sign (-1, 0, 1) |
| `Math.clamp(x, min, max)` | Clamp to range (TC39 proposal) |
| `Math.exp(x)` | e^x |
| `Math.log(x)` | Natural logarithm |
| `Math.log10(x)` | Base-10 logarithm |
| `Math.sin(x)` | Sine |
| `Math.cos(x)` | Cosine |
| `Math.tan(x)` | Tangent |
| `Math.acos(x)` | Arc cosine |
| `Math.asin(x)` | Arc sine |
| `Math.atan(x)` | Arc tangent |
| `Math.atan2(y, x)` | Arc tangent of y/x |
| `Math.cbrt(x)` | Cube root |
| `Math.cosh(x)` | Hyperbolic cosine |
| `Math.sinh(x)` | Hyperbolic sine |
| `Math.tanh(x)` | Hyperbolic tangent |
| `Math.acosh(x)` | Inverse hyperbolic cosine |
| `Math.asinh(x)` | Inverse hyperbolic sine |
| `Math.atanh(x)` | Inverse hyperbolic tangent |
| `Math.expm1(x)` | e^x - 1 (precise for small x) |
| `Math.f16round(x)` | Nearest 16-bit float |
| `Math.fround(x)` | Nearest 32-bit float |
| `Math.hypot(x, y)` | Square root of sum of squares |
| `Math.imul(a, b)` | 32-bit integer multiplication |
| `Math.log1p(x)` | ln(1 + x) (precise for small x) |
| `Math.log2(x)` | Base-2 logarithm |
| `Math.clz32(x)` | Count leading zeros (32-bit) |
| `Math.sumPrecise(iterable)` | Precise sum of iterable of numbers (TC39 proposal) |

All methods handle `NaN` and `Infinity` edge cases correctly.

`Math.sumPrecise(iterable)` takes an iterable of numbers and returns their sum using Kahan-Babuska-Neumaier compensated summation, avoiding floating-point precision loss from naive addition. Throws `TypeError` if the argument is not iterable (e.g., `null`, `undefined`, numbers, plain objects without `[Symbol.iterator]`). Non-number values in the iterable also throw `TypeError`. An empty iterable returns `-0`. If any element is `NaN`, returns `NaN`. Mixed `+Infinity` and `-Infinity` returns `NaN`.

### JSON (`Goccia.Builtins.JSON.pas`)

| Method | Description |
|--------|-------------|
| `JSON.parse(text, reviver?)` | Parse JSON string to value, with optional reviver function that receives source text access |
| `JSON.stringify(value, replacer?, space?)` | Convert value to JSON string, with optional replacer (function or array) and indentation |

**Source text access (ES2024):** When a reviver is provided, it receives three arguments: `(key, value, context)`. The `context` parameter is an object. For primitive JSON values (numbers, strings, booleans, `null`), the context contains a `source` property with the raw JSON text that produced the value — including quotes for strings, exact numeric notation, and escape sequences as written. For objects and arrays, the context object has no `source` property. This enables lossless round-tripping of numeric precision and format-aware value reconstruction.

The JSON parser is a recursive descent implementation. Special handling:
- `NaN` → `null` in stringify
- `undefined` → omitted in objects, `null` in arrays
- Functions → omitted in objects, `null` in arrays
- Symbols → omitted in objects, `null` in arrays
- Finite floating-point numbers are serialized with round-trip-safe decimal text when needed
- `toJSON()` is called before serializing object values
- Circular references throw `TypeError`

### JSON5 (`Goccia.Builtins.JSON5.pas`)

| Method | Description |
|--------|-------------|
| `JSON5.parse(text, reviver?)` | Parse JSON5 text to a value, with optional reviver function that receives source text access |
| `JSON5.stringify(value, replacerOrOptions?, space?)` | Serialize a value with JSON5 syntax, including special numeric values and optional `quote` override |

`JSON5.parse` delegates to the standalone `TGocciaJSON5Parser` utility in `Goccia.JSON5`. The JSON5 parser shares the same core capability-driven parser engine as strict JSON, so `JSON.parse(...)` stays strict while `JSON5.parse(...)` opts into comments, trailing commas, single-quoted strings, unquoted identifier keys, hexadecimal numbers, signed numbers, `Infinity`, `NaN`, line continuations, and ECMAScript whitespace extensions. Source text access works identically to `JSON.parse`: the reviver receives `(key, value, context)` where `context.source` preserves the raw JSON5 text for primitives (including extended literals like `Infinity`, `NaN`, `0xFF`, and `+1`).

`JSON5.stringify` delegates to `TGocciaJSON5Stringifier`, which reuses the same shared serialization core as strict JSON but switches to JSON5 formatting rules. That means unquoted identifier keys, single- or double-quoted strings (with optional `{ quote: "'" | '"' }` override), preserved `Infinity` / `-Infinity` / `NaN`, trailing commas when pretty-printing, `toJSON5()` preference over `toJSON()`, and the same replacer / space semantics as JSON plus the upstream JSON5 options-object form `{ replacer, space, quote }`.

Compatibility goal: GocciaScript is targeting full JSON5 parser compatibility plus upstream-aligned stringify behavior. `python3 scripts/run_json5_test_suite.py` now runs both the official `json5/json5` parser corpus and the local upstream-aligned stringify suite in one command, and `python3 scripts/run_json5_test_suite.py --harness=./build/GocciaJSON5Check` reuses a prebuilt parser decoder when you already have it.

### YAML (`Goccia.Builtins.YAML.pas`)

| Method | Description |
|--------|-------------|
| `YAML.parse(text)` | Parse YAML text; returns an array when the stream uses explicit `---` document markers |
| `YAML.parseDocuments(text)` | Parse a YAML stream and always return an array of documents |

`YAML.parse` delegates to the standalone `TGocciaYAMLParser` utility in `Goccia.YAML`, mirroring the JSON built-in's split between parser utility and global runtime surface. Its behavior now matches Bun's YAML runtime semantics: when the input uses explicit `---` document markers, `YAML.parse(...)` returns an array of parsed documents; otherwise it returns the first parsed document value directly. `YAML.parseDocuments(...)` is the always-array variant.

The current YAML parser also supports anchors, aliases, merge keys, self-referential alias graphs for mappings and sequences, multiline flow-style collections and flow edge cases such as single-pair mapping entries, empty implicit keys, and trailing commas, block scalars (`|`, `>`, chomping modifiers, and indentation indicators), multiline plain and quoted scalar folding, YAML 1.2 numeric scalar resolution (including base-prefixed integers, exponent forms, and validated digit separators), YAML double-quoted escapes (`\x`, `\u`, `\U`, line continuations, and YAML-specific escapes), `%YAML` / `%TAG` directives, and the standard tags `!!str`, `!!int`, `!!float`, `!!bool`, `!!null`, `!!seq`, `!!map`, `!!timestamp`, and `!!binary`. Directives are treated as document-preamble syntax and are rejected if they appear after document content without a document boundary.

Tagged values preserve runtime metadata through `.tagName` and `.value`. Custom tags wrap the parsed underlying value, `!!timestamp` validates ISO date/date-time scalars, and `!!binary` validates and decodes base64 text into the wrapped scalar value.

Explicit keys (`? key`) are supported, including omitted explicit values and zero-indented sequence values. Because GocciaScript mappings are backed by string-keyed objects, non-scalar YAML keys are canonicalized into stable JSON-like strings during parsing, and anchored mapping keys now parse instead of being rejected outright.

Compatibility goal: GocciaScript is targeting full YAML 1.2 support over time while keeping Bun-compatible YAML runtime behavior where practical. The current parser is still a partial implementation. The detailed conformance snapshot lives in [docs/design-decisions.md](design-decisions.md), and the official parse-validity check can be rerun with `python3 scripts/run_yaml_test_suite.py`.

### JSONL (`Goccia.Builtins.JSONL.pas`)

| Method | Description |
|--------|-------------|
| `JSONL.parse(textOrBytes)` | Parse newline-delimited JSON and return an array of all records |
| `JSONL.parseChunk(textOrBytes[, start[, end]])` | Parse as many complete JSONL records as possible and return `{ values, read, done, error }` |

`JSONL.parse(...)` and `JSONL.parseChunk(...)` are designed to match Bun's JSONL runtime surface closely: blank lines are ignored, each non-empty line must be strict JSON, and `Uint8Array` input is supported alongside strings. `parseChunk(...)` returns the next resume offset in `read`, a `done` flag when the provided range was fully consumed, and a `SyntaxError` object in `error` when a delimited record is invalid. Incomplete trailing records are left unread so callers can append more data and resume parsing.

### TOML (`Goccia.Builtins.TOML.pas`)

| Method | Description |
|--------|-------------|
| `TOML.parse(text)` | Parse TOML 1.1.0 text into Goccia values |

`TOML.parse` delegates to the standalone `TGocciaTOMLParser` utility in `Goccia.TOML`, mirroring the JSON and YAML split between parser utility and runtime surface. The current TOML surface supports strings (basic, literal, and multiline variants), integers, floats, booleans, arrays, inline tables, regular tables, arrays of tables, dotted keys, and TOML 1.1.0 date/time values. TOML multiline strings normalize recognized source newlines to LF (`\n`) regardless of the host platform, so the parsed value is stable across Linux, macOS, and Windows. File-backed TOML inputs are treated as UTF-8 text on every platform, and the raw file text stays `UTF8String` until the parser consumes it. That type choice is intentional: in this FreePascal configuration plain `string` is still `AnsiString`, while `UTF8String` is the explicit UTF-8-tagged ansistring we use to preserve non-ASCII keys and values across Windows and non-Windows targets.

TOML date/time values currently map to validated string scalars rather than Temporal values. This keeps the runtime and module-import behavior stable for v1 while leaving room for future Temporal-aware interop.

Compatibility goal: GocciaScript is targeting full TOML 1.1.0 support over time. The detailed conformance notes live in [docs/design-decisions.md](design-decisions.md), and the official `toml-test` rerun command is `python3 scripts/run_toml_test_suite.py` or `python3 scripts/run_toml_test_suite.py --harness=./build/GocciaTOMLCheck` when you already have the decoder harness built.

### Object (`Goccia.Builtins.GlobalObject.pas`)

| Method | Description |
|--------|-------------|
| `Object.keys(obj)` | Own enumerable property names |
| `Object.values(obj)` | Own enumerable property values |
| `Object.entries(obj)` | Own enumerable [key, value] pairs |
| `Object.assign(target, ...sources)` | Copy properties |
| `Object.create(proto)` | Create with prototype |
| `Object.is(a, b)` | Same-value equality (handles -0, NaN) |
| `Object.hasOwn(obj, prop)` | Own property check (supports class values; checks static properties, not private fields) |
| `Object.defineProperty(obj, prop, desc)` | Define/update property descriptor (merges with existing) |
| `Object.defineProperties(obj, props)` | Define multiple descriptors |
| `Object.getOwnPropertyNames(obj)` | All own property names |
| `Object.getOwnPropertyDescriptor(obj, prop)` | Get property descriptor |
| `Object.getOwnPropertySymbols(obj)` | All own symbol-keyed properties |
| `Object.freeze(obj)` | Freeze object (make all properties non-writable/non-configurable) |
| `Object.isFrozen(obj)` | Check if object is frozen |
| `Object.getPrototypeOf(obj)` | Get the prototype of an object |
| `Object.fromEntries(entries)` | Create object from `[[key, value], ...]` array |
| `Object.seal(obj)` | Seal object (prevent new properties, keep existing writable) |
| `Object.isSealed(obj)` | Check if object is sealed |
| `Object.preventExtensions(obj)` | Prevent new properties from being added |
| `Object.isExtensible(obj)` | Check if object is extensible |
| `Object.setPrototypeOf(obj, proto)` | Set the prototype of an object |
| `Object.groupBy(iterable, callback)` | Group elements by callback return value |

**Prototype methods (registered on `Object.prototype` in `Goccia.Engine.pas`):**

| Method | Description |
|--------|-------------|
| `Object.prototype.toString()` | Returns `[object Tag]` where Tag is `Symbol.toStringTag` if present, or the built-in type tag (e.g. `Object`, `Array`, `Function`, `Set`, `Map`, `Promise`, `ArrayBuffer`). Handles primitives via `.call()`: `undefined` → `[object Undefined]`, `null` → `[object Null]`, booleans → `[object Boolean]`, numbers → `[object Number]`, strings → `[object String]`, symbols → `[object Symbol]`. |
| `Object.prototype.hasOwnProperty(V)` | Returns `true` if the object has the named own property (not inherited). Property key is coerced to string. |
| `Object.prototype.isPrototypeOf(V)` | Walks `V`'s prototype chain to check if `this` appears in it. Returns `false` for non-object arguments. |
| `Object.prototype.propertyIsEnumerable(V)` | Returns `true` if the named own property exists and has the enumerable flag set. |
| `Object.prototype.toLocaleString()` | Calls `this.toString()` and returns the result. |
| `Object.prototype.valueOf()` | Returns `this` (identity for objects). |

### Array (`Goccia.Builtins.GlobalArray.pas`)

**Static methods:**

| Method | Description |
|--------|-------------|
| `Array.isArray(value)` | Check if value is an array |
| `Array.from(iterable, mapFn?)` | Create array from iterable (array or string) with optional map |
| `Array.fromAsync(asyncItems [, mapfn [, thisArg]])` | Returns a Promise that resolves to an Array. Tries `[Symbol.asyncIterator]` first, falls back to `[Symbol.iterator]`, then array-like. Each value is awaited (Promise-aware via synchronous microtask drain). |
| `Array.of(...items)` | Create array from arguments |

**Prototype methods** (implemented directly on `TGocciaArrayValue`):

| Method | Description |
|--------|-------------|
| `map(callback)` | Transform each element |
| `filter(callback)` | Filter elements by predicate |
| `reduce(callback, initial?)` | Reduce to single value |
| `forEach(callback)` | Execute callback for each element |
| `some(callback)` | Test if any element passes |
| `every(callback)` | Test if all elements pass |
| `flat(depth?)` | Flatten nested arrays |
| `flatMap(callback)` | Map then flatten one level |
| `find(callback)` | Find first matching element |
| `findIndex(callback)` | Find index of first match |
| `findLast(callback)` | Find last matching element |
| `findLastIndex(callback)` | Find index of last match |
| `indexOf(value, fromIndex?)` | Find index of value |
| `lastIndexOf(value, fromIndex?)` | Find last index of value |
| `includes(value, fromIndex?)` | Check if array contains value |
| `join(separator?)` | Join elements into string |
| `toString()` | Returns comma-separated string (delegates to `join()`) |
| `concat(...arrays)` | Concatenate arrays |
| `slice(start?, end?)` | Extract a section |
| `push(...items)` | Add to end (mutating) |
| `pop()` | Remove from end (mutating) |
| `shift()` | Remove from start (mutating) |
| `unshift(...items)` | Add to start (mutating) |
| `sort(compareFn?)` | Sort in place (mutating) |
| `splice(start, deleteCount?, ...items)` | Add/remove elements (mutating) |
| `reverse()` | Reverse in place (mutating) |
| `fill(value, start?, end?)` | Fill with value (mutating) |
| `at(index)` | Access element (supports negative index) |
| `toReversed()` | Non-mutating reverse |
| `toSorted(compareFn?)` | Non-mutating sort |
| `with(index, value)` | Non-mutating element replacement |
| `copyWithin(target, start, end?)` | Copy within array (mutating) |
| `toSpliced(start, deleteCount?, ...items)` | Non-mutating splice |
| `values()` | Returns an iterator over values |
| `keys()` | Returns an iterator over indices |
| `entries()` | Returns an iterator over `[index, value]` pairs |
| `[Symbol.iterator]()` | Returns a values iterator (same as `values()`) |

### Number (`Goccia.Builtins.GlobalNumber.pas`)

**Constants:**

| Constant | Value |
|----------|-------|
| `Number.NaN` | Not-a-Number |
| `Number.POSITIVE_INFINITY` | Positive infinity |
| `Number.NEGATIVE_INFINITY` | Negative infinity |
| `Number.MAX_SAFE_INTEGER` | 2^53 - 1 (9007199254740991) |
| `Number.MIN_SAFE_INTEGER` | -(2^53 - 1) |
| `Number.MAX_VALUE` | Largest representable number |
| `Number.MIN_VALUE` | Smallest positive number |
| `Number.EPSILON` | Smallest difference between 1 and next float |

**Methods:**

| Method | Description |
|--------|-------------|
| `Number.parseInt(str, radix?)` | Parse integer (supports radix 2-36) |
| `Number.parseFloat(str)` | Parse floating-point |
| `Number.isFinite(value)` | Check if finite number |
| `Number.isNaN(value)` | Check if NaN |
| `Number.isInteger(value)` | Check if integer |
| `Number.isSafeInteger(value)` | Check if safe integer (within ±2^53-1) |

These are **not** available as global functions — see [language-restrictions.md](language-restrictions.md#no-global-parseint-parsefloat-isnan-isfinite) for the rationale and polyfill pattern.

**Prototype methods** (available on number values via auto-boxing):

| Method | Description |
|--------|-------------|
| `num.toFixed(digits?)` | Format with fixed-point notation (0-100 digits, default 0) |
| `num.toString(radix?)` | Convert to string (supports radix 10 and 16) |
| `num.valueOf()` | Return the primitive number value |
| `num.toPrecision(precision?)` | Format to specified significant digits |
| `num.toExponential(fractionDigits?)` | Format in exponential notation |

All prototype methods correctly handle special values — `NaN`, `Infinity`, `-Infinity`, and `-0` return their standard string representations rather than attempting numeric formatting.

### String (`Goccia.Builtins.GlobalString.pas`)

String constructor available as `String()`.

**Static methods:**

| Method | Description |
|--------|-------------|
| `String.fromCharCode(...codes)` | Create string from UTF-16 code units |
| `String.fromCodePoint(...codePoints)` | Create string from Unicode code points |
| `String.raw(template, ...substitutions)` | Create string from raw template strings (escape sequences not processed) |

String prototype methods are implemented on string values:

`charAt`, `charCodeAt`, `codePointAt`, `indexOf`, `lastIndexOf`, `includes`, `startsWith`, `endsWith`, `slice`, `substring`, `toLowerCase`, `toUpperCase`, `trim`, `trimStart`, `trimEnd`, `repeat`, `replace`, `replaceAll`, `split`, `match`, `matchAll`, `search`, `padStart`, `padEnd`, `concat`, `at`, `localeCompare`, `normalize`, `isWellFormed`, `toWellFormed`, `[Symbol.iterator]`

`replace`, `replaceAll`, `split`, `match`, `matchAll`, and `search` also honor custom objects implementing the corresponding `Symbol.replace`, `Symbol.split`, `Symbol.match`, `Symbol.matchAll`, or `Symbol.search` hooks.

`[Symbol.iterator]()` returns an iterator that yields individual characters.

### RegExp (`Goccia.Builtins.GlobalRegExp.pas`)

RegExp is available as both `RegExp()` and `new RegExp()`. Regex literals (`/pattern/flags`) are also supported in both interpreted and bytecode execution modes. Calling `RegExp(existingRegExp)` without `new` and without an explicit `flags` argument returns the original regex object; `new RegExp(existingRegExp)` clones it.

**Supported flags:** `d`, `g`, `i`, `m`, `s`, `u`, `v`, `y`

**Constructor properties:**

| Property | Description |
|----------|-------------|
| `source` | Normalized pattern source |
| `flags` | Canonicalized flags string (sorted: `dgimsuvy`) |
| `lastIndex` | Current match position for `g`/`y` regexes |
| `global` | `true` when the `g` flag is present |
| `ignoreCase` | `true` when the `i` flag is present |
| `multiline` | `true` when the `m` flag is present |
| `dotAll` | `true` when the `s` flag is present |
| `unicode` | `true` when the `u` flag is present |
| `sticky` | `true` when the `y` flag is present |
| `unicodeSets` | `true` when the `v` flag is present |
| `hasIndices` | `true` when the `d` flag is present |

**Static methods:**

| Method | Description |
|--------|-------------|
| `escape(string)` | Returns a string with all regex-significant characters escaped so the result can be safely interpolated into a pattern. Implements the TC39 RegExp Escaping proposal. Syntax characters are backslash-escaped; ClassSet-reserved punctuators, whitespace, and line terminators are hex-encoded (`\xHH` / `\uHHHH`). A leading digit or ASCII letter is also hex-encoded to prevent ambiguity with backreferences. |

**Prototype methods:**

| Method | Description |
|--------|-------------|
| `exec(string)` | Returns a match array with capture groups, `index`, `input`, and `groups`, or `null` |
| `test(string)` | Returns `true` if the regex matches |
| `toString()` | Returns `/pattern/flags` |
| `[Symbol.match](string)` | Matching hook used by `String.prototype.match()` |
| `[Symbol.matchAll](string)` | Match-all hook used by `String.prototype.matchAll()` |
| `[Symbol.replace](string, replaceValue)` | Replacement hook used by `String.prototype.replace()` and `replaceAll()` |
| `[Symbol.search](string)` | Search hook used by `String.prototype.search()` |
| `[Symbol.split](string, limit?)` | Split hook used by `String.prototype.split()` |

**Behavior notes:**

- **Named capture groups** (`(?<name>...)`) are supported. Match results include a `groups` property (an object with `null` prototype) mapping group names to matched strings. Non-participating named groups are `undefined`. When no named groups exist, `groups` is `undefined`.
- **Duplicate named capture groups** (ES2025): the same group name may appear in different alternatives of a disjunction (`|`). For example, `/(?<year>\d{4})-\d{2}|\d{2}-(?<year>\d{4})/`. The `groups` property on the match result returns the value from whichever alternative participated; non-participating duplicates are `undefined`. Using the same name in the same alternative (where both groups could participate) is a `SyntaxError`.
- **Named backreferences** (`\k<name>`) reference a previously captured named group within the same pattern. With duplicate named capture groups, `\k<name>` resolves to the group with that name in the same alternative branch.
- Replacement strings in regex-backed `replace()` and `replaceAll()` support `$$`, `$&`, the pre-match token `$` followed by a backtick, `$'`, numeric captures such as `$1`, and **named group references** `$<name>`. An unmatched `$<name>` produces an empty string; `$<` without a closing `>` is literal.
- When the replacer is a function and named groups are present, the `groups` object is passed as the last argument after `input`.
- `String.prototype.match`, `matchAll`, `replace`, `replaceAll`, `search`, and `split` dispatch through the corresponding well-known symbol hooks, so custom protocol objects work as expected.
- `matchAll()` returns a lazy iterator that advances matches on demand per the ES2026 spec.
- The `u` flag enables Unicode-aware pattern matching. Unicode property escapes (`\p{Letter}`, `\P{ASCII}`, etc.) are expanded to equivalent character classes. Unicode code point escapes (`\u{41}`, `\u{1F600}`) are converted to UTF-8 byte sequences. Supported properties: `L`/`Letter`, `Lu`/`Uppercase_Letter`, `Ll`/`Lowercase_Letter`, `N`/`Number`, `Nd`/`Decimal_Number`, `P`/`Punctuation`, `S`/`Symbol`, `Z`/`Separator`, `Cc`/`Control`, `ASCII`, `ASCII_Hex_Digit`, `White_Space`. Unsupported properties throw `SyntaxError`. The `u` flag also disables TRegExpr's Russian charset extensions and enables correct `AdvanceStringIndex` for multi-byte UTF-8 sequences.
- The `v` flag (Unicode sets) is accepted and exposed through `.flags` and `.unicodeSets`. The `u` and `v` flags are mutually exclusive. Full Unicode set notation and properties of strings in character classes are not yet implemented beyond basic `u` flag behavior.
- The `d` flag (indices) is accepted and exposed through `.flags` and `.hasIndices`. Match indices are not yet populated.

### Global Constants, Functions, and Error Constructors (`Goccia.Builtins.Globals.pas`)

These are always registered (not flag-gated).

**Constants:** `undefined`, `NaN`, `Infinity`, `globalThis`

`globalThis` is a `const` binding that holds a plain object populated with all global scope bindings at the time of registration. It includes a self-referential `globalThis` property (`globalThis.globalThis === globalThis`).

**`Goccia` object:**

A `const` global providing engine metadata and Goccia-owned utility APIs:

| Property | Type | Description |
|----------|------|-------------|
| `version` | `string` | Semver version from the latest git tag (e.g., `"0.2.0"`), or tag + `-dev` suffix if there are commits after the tag (e.g., `"0.2.0-dev"`) |
| `commit` | `string` | Short git commit hash (e.g., `"a1b2c3d"`) |
| `strictTypes` | `boolean` | Configurable at engine creation. Defaults to `false` for interpreter, `true` for bytecode. Controls type enforcement for annotated variables. |
| `build` | `object` | Compile-time platform information (see below) |
| `semver` | `object` | SemVer 2.0.0 API namespace (see below) |
| `spec` | `object` | ES specification features implemented by GocciaScript, keyed by year (e.g., `"2015"`, `"2025"`). Each year maps to an array of `{ name, link }` entries. |
| `proposal` | `object` | TC39 proposals implemented by GocciaScript, keyed by stage (e.g., `"stage-3"`, `"stage-1"`). Each stage maps to an array of `{ name, link }` entries. |
| `shims` | `string[]` | Names of registered shims (currently empty, infrastructure for future shim support) |

**`Goccia.build`**

`Goccia.build` exposes compile-time platform information, mirroring `Deno.build`:

| Property | Type | Description |
|----------|------|-------------|
| `os` | `string` | Operating system: `"darwin"`, `"linux"`, `"windows"`, `"freebsd"`, `"netbsd"`, `"openbsd"`, `"android"`, `"aix"`, `"solaris"`, or `"unknown"` |
| `arch` | `string` | Processor architecture: `"x86_64"`, `"aarch64"`, `"x86"`, `"arm"`, `"powerpc64"`, `"powerpc"`, or `"unknown"` |

**`Goccia.semver`**

`Goccia.semver` exposes a SemVer 2.0.0 API modeled after the main `node-semver` export plus its documented module-group aliases:

| Property | Type | Description |
|----------|------|-------------|
| `SEMVER_SPEC_VERSION` | `string` | Always `"2.0.0"` |
| `RELEASE_TYPES` | `string[]` | `["major", "premajor", "minor", "preminor", "patch", "prepatch", "prerelease"]` |
| `SemVer` | `function` | Constructor for parsed semantic versions |
| `Comparator` | `function` | Constructor for comparator objects |
| `Range` | `function` | Constructor for range objects |
| `classes` | `object` | Aliases for `SemVer`, `Comparator`, and `Range` matching the documented `node-semver` class module groups |
| `functions` | `object` | Aliases for the documented function-level exports (`valid`, `clean`, `compare`, `inc`, etc.) |
| `ranges` | `object` | Aliases for the documented range helpers (`gtr`, `ltr`, `maxSatisfying`, `valid`, etc.) |

Top-level methods follow `node-semver` naming and nullability conventions. The currently exposed main-export surface is:

`valid`, `clean`, `parse`, `inc`, `prerelease`, `major`, `minor`, `patch`, `intersects`, `gt`, `gte`, `lt`, `lte`, `eq`, `neq`, `cmp`, `compare`, `rcompare`, `compareBuild`, `compareLoose`, `diff`, `sort`, `rsort`, `validRange`, `satisfies`, `maxSatisfying`, `minSatisfying`, `minVersion`, `gtr`, `ltr`, `outside`, `simplifyRange`, `subset`, `toComparators`, `coerce`.

The constructor-backed objects mirror the `node-semver` public fields and core instance methods:

| Constructor | Instance fields | Instance methods |
|-------------|-----------------|------------------|
| `new Goccia.semver.SemVer(version, options?)` | `raw`, `version`, `major`, `minor`, `patch`, `prerelease`, `build`, `options`, `loose`, `includePrerelease` | `format()`, `toString()`, `compare(other)`, `compareMain(other)`, `comparePre(other)`, `compareBuild(other)`, `inc(releaseType, identifier?)` |
| `new Goccia.semver.Comparator(comparator, options?)` | `operator`, `semver`, `value`, `options`, `loose` | `toString()`, `test(version)`, `intersects(other, options?)` |
| `new Goccia.semver.Range(range, options?)` | `raw`, `range`, `set`, `options`, `loose`, `includePrerelease` | `format()`, `toString()`, `test(version)`, `intersects(other, options?)` |

**Global functions:**

| Function | Description |
|----------|-------------|
| `queueMicrotask(callback)` | Enqueue a callback to run as a microtask. Throws `TypeError` if the argument is not callable. |
| `structuredClone(value)` | Deep-clone a value using the structured clone algorithm. Handles objects, arrays, `Map`, `Set`, and circular references. Throws `DOMException` with name `"DataCloneError"` (code 25) for non-cloneable types (functions, symbols). |
| `btoa(data)` | Encode a binary string (each character code ≤ U+00FF) to base64. Throws `DOMException` with name `"InvalidCharacterError"` (code 5) if any character code exceeds U+00FF. |
| `atob(data)` | Decode a base64 string to a binary string. Uses WHATWG forgiving-base64-decode: strips ASCII whitespace, tolerates missing `=` padding. Throws `DOMException` with name `"InvalidCharacterError"` (code 5) for invalid base64 input. |
| `encodeURI(uriString)` | Encode a complete URI, preserving reserved characters (`;/?:@&=+$,#`) and unreserved characters. Multi-byte characters are UTF-8 encoded. Throws `URIError` for lone surrogates. |
| `decodeURI(encodedURI)` | Decode a percent-encoded URI. Does **not** decode reserved characters (`%3B`, `%2F`, etc.). Throws `URIError` for malformed percent sequences or invalid UTF-8. |
| `encodeURIComponent(uriComponent)` | Encode a URI component. Only unreserved characters (`A-Z a-z 0-9 - _ . ! ~ * ' ( )`) are preserved. Throws `URIError` for lone surrogates. |
| `decodeURIComponent(encodedURIComponent)` | Decode a percent-encoded URI component. Decodes all percent sequences including reserved characters. Throws `URIError` for malformed percent sequences or invalid UTF-8. |

`queueMicrotask` shares the same microtask queue used by Promise reactions. Callbacks run after the current synchronous code completes but before the engine returns control. If a callback throws, the error is silently discarded and remaining microtasks still execute.

`structuredClone` creates a deep copy following the HTML spec's structured clone algorithm. Primitives are returned as-is. Objects, arrays, Maps, and Sets are recursively cloned. Circular references and shared references within the object graph are preserved (the same cloned object is reused). Non-serializable values (functions, symbols) throw a `DOMException` with `name: "DataCloneError"` and `code: 25`, matching browser and Node.js behavior. Accessor properties (getters/setters) are read via the getter and the resulting value is cloned as a data property on the clone.

`btoa` encodes a string to base64 following the WHATWG HTML spec §8.3. Each character in the input must have a code point ≤ U+00FF (Latin-1 range); characters outside this range throw a `DOMException` with name `"InvalidCharacterError"` and legacy code 5. The input is interpreted as a byte sequence where each code point maps 1:1 to a byte value.

`atob` decodes a base64 string following the WHATWG forgiving-base64-decode algorithm. ASCII whitespace (U+0009, U+000A, U+000C, U+000D, U+0020) is stripped before decoding. Missing `=` padding is tolerated. Invalid characters or an invalid length (length mod 4 = 1 after cleanup) throw a `DOMException` with name `"InvalidCharacterError"` and legacy code 5. The decoded bytes are returned as a string where each byte becomes a character (Latin-1 interpretation).

`encodeURI` / `decodeURI` / `encodeURIComponent` / `decodeURIComponent` implement ES2026 §19.2.6. The shared encoding/decoding logic lives in `Goccia.URI.pas` and is also used by `import.meta.url` for file-path percent-encoding. Multi-byte Unicode characters are encoded as UTF-8 octets, each percent-encoded individually (e.g., `encodeURIComponent("中")` → `%E4%B8%AD`). Lone surrogates (U+D800–U+DFFF) throw `URIError`. Decoding validates UTF-8 well-formedness: overlong encodings, truncated sequences, and code points above U+10FFFF all throw `URIError`. `decodeURI` re-emits reserved characters as uppercase percent-encoded sequences even when the input uses lowercase hex digits (e.g., `%2f` → `%2F`).

**Error constructors:** `Error`, `TypeError`, `ReferenceError`, `RangeError`, `SyntaxError`, `URIError`, `AggregateError`, `DOMException`

**Prototype chain:** All error types follow the ES2026 prototype hierarchy. `TypeError.prototype`, `RangeError.prototype`, etc. inherit from `Error.prototype`. This means `new TypeError("msg") instanceof TypeError` is `true` AND `new TypeError("msg") instanceof Error` is `true`. Cross-type checks return `false` (e.g., `new TypeError("msg") instanceof RangeError` is `false`).

**Runtime errors:** Errors thrown internally by the engine (e.g., `TypeError` from accessing a property on `null`/`undefined`, or `RangeError` from invalid `ArrayBuffer` length) have the same prototype chain as user-constructed errors. This means `instanceof` checks work correctly in catch blocks:

```javascript
try { null.x; } catch (e) {
  e instanceof TypeError; // true
  e instanceof Error;     // true
}
```

**Static methods:**

| Method | Description |
|--------|-------------|
| `Error.isError(arg)` | Returns `true` if `arg` is an Error instance (has `[[ErrorData]]` internal slot), `false` otherwise (ES2026) |

All error constructors accept an optional second argument `options` with a `cause` property. `AggregateError` takes `(errors, message, options?)` where `errors` is an array of error objects. `DOMException` takes `(message?, name?)` where `name` defaults to `"Error"` — the `code` property is automatically set from the legacy error code mapping (e.g., `"DataCloneError"` → 25).

Each creates an error object with `name`, `message`, and `stack` properties. The `stack` property is a formatted string with the following structure:

```
ErrorName: message
    at functionName (filePath:line:col)
    at outerFunction (filePath:line:col)
```

### Iterator (`Goccia.Values.IteratorValue.pas`, `Iterator.Concrete.pas`, `Iterator.Lazy.pas`, `Iterator.Concat.pas`, `Iterator.Generic.pas`)

All built-in iterators (Array, String, Map, Set) share a common `Iterator.prototype` with helper methods per the ECMAScript Iterator Helpers proposal:

| Method | Description |
|--------|-------------|
| `iter.next()` | Returns `{value, done}` — advances the iterator |
| `iter[Symbol.iterator]()` | Returns `iter` itself (iterators are iterable) |
| `iter.map(callback)` | Returns a new iterator with mapped values |
| `iter.filter(callback)` | Returns a new iterator with filtered values |
| `iter.take(n)` | Returns a new iterator with the first `n` values |
| `iter.drop(n)` | Returns a new iterator skipping the first `n` values |
| `iter.flatMap(callback)` | Map and flatten one level |
| `iter.forEach(callback)` | Execute callback for each value |
| `iter.reduce(callback, initial?)` | Reduce to single value |
| `iter.toArray()` | Collect remaining values into an array |
| `iter.some(callback)` | Test if any value matches |
| `iter.every(callback)` | Test if all values match |
| `iter.find(callback)` | Find first matching value |

**Lazy helpers:** `map`, `filter`, `take`, `drop`, and `flatMap` return new lazy iterators that advance the source on-demand — they do not eagerly consume the underlying iterator. Consuming methods (`forEach`, `reduce`, `toArray`, `some`, `every`, `find`) do consume the iterator.

Iterators are consumed once — calling `next()` past the end returns `{value: undefined, done: true}` forever. Spread (`[...iter]`), destructuring, and `Array.from()` all consume iterators via the `[Symbol.iterator]` protocol.

**User-defined iterables:** Objects with a `[Symbol.iterator]()` method that returns a plain `{next()}` object are fully supported. They work with spread, destructuring, `Array.from()`, and `Iterator.from()`.

**Static methods:**

| Method | Description |
|--------|-------------|
| `Iterator.from(value)` | Wrap an iterable, iterator, or `{next()}` object as a proper Iterator with helper methods |
| `Iterator.concat(...items)` | Concatenate multiple iterables into a single lazy iterator (TC39 Iterator Sequencing). All arguments are validated upfront as iterables; iterators are opened lazily as each iterable is consumed. |
| `Iterator.zip(iterables, options?)` | Combine multiple iterables into an iterator of arrays (TC39 Joint Iteration). `iterables` is an iterable of iterables. `options.mode` can be `"shortest"` (default), `"longest"`, or `"strict"`. In `"longest"` mode, `options.padding` (an iterable) provides fill values for exhausted iterables. |
| `Iterator.zipKeyed(iterables, options?)` | Combine keyed iterables into an iterator of objects (TC39 Joint Iteration). `iterables` is an object whose values are iterables. Each yielded object has the same keys as the input. Same `mode`/`padding` options as `zip`, but `padding` is an object with matching keys. |
| `Iterator.prototype` | The shared iterator prototype (accessible for inspection) |

### Symbol (`Goccia.Builtins.GlobalSymbol.pas`)

Symbols are unique, immutable primitive values used as property keys.

| Method/Property | Description |
|--------|-------------|
| `Symbol(description?)` | Create a new unique symbol |
| `Symbol.for(key)` | Get/create a symbol in the global registry |
| `Symbol.keyFor(symbol)` | Get the key for a global registry symbol (throws `TypeError` for non-symbol arguments) |
| `Symbol.iterator` | Well-known symbol for iteration protocol |
| `Symbol.asyncIterator` | Well-known symbol for async iteration protocol. Returns the singleton symbol. Used by `for await...of` and `Array.fromAsync`. Objects with a `[Symbol.asyncIterator]()` method are async iterables. |
| `Symbol.match` | Well-known symbol for `String.prototype.match()` dispatch |
| `Symbol.matchAll` | Well-known symbol for `String.prototype.matchAll()` dispatch |
| `Symbol.replace` | Well-known symbol for `String.prototype.replace()` and `replaceAll()` dispatch |
| `Symbol.search` | Well-known symbol for `String.prototype.search()` dispatch |
| `Symbol.split` | Well-known symbol for `String.prototype.split()` dispatch |
| `Symbol.species` | Well-known symbol for constructor species (behavioral: Array/Map/Set constructors have default `[Symbol.species]` getter returning `this`; Array prototype methods use `ArraySpeciesCreate` for subclass-aware result construction) |
| `Symbol.hasInstance` | Well-known symbol for instanceof behavior |
| `Symbol.toPrimitive` | Well-known symbol for type conversion |
| `Symbol.toStringTag` | Well-known symbol for Object.prototype.toString |
| `Symbol.isConcatSpreadable` | Well-known symbol for Array.prototype.concat |
| `Symbol.metadata` | Well-known symbol for decorator metadata (TC39 proposal-decorator-metadata) |
| `symbol.toString()` | Returns `"Symbol(description)"` |
| `symbol.description` | The description string, or `undefined` |

Symbols can be used as computed property keys:

```javascript
const sym = Symbol("myKey");
const obj = { [sym]: "value" };
obj[sym]; // "value"
```

`Object.defineProperty` and `Object.getOwnPropertySymbols` also support symbol keys. The `in` operator checks for symbol-keyed properties, including global registry symbols created via `Symbol.for()`.

**Prototype:** `Symbol.prototype` is an object containing `toString()` and a `description` getter, matching ECMAScript semantics. All symbol instances share this prototype.

**Coercion semantics:** Implicit conversion of a symbol to string or number throws `TypeError`. Use `String(symbol)` or `symbol.toString()` for explicit string conversion. See [value-system.md](value-system.md#symbols) for details.

### Set (`Goccia.Builtins.GlobalSet.pas`)

A collection of unique values with insertion-order iteration.

| Method/Property | Description |
|--------|-------------|
| `new Set(iterable?)` | Create a new Set, optionally from an array |
| `set.add(value)` | Add a value (returns the Set for chaining) |
| `set.has(value)` | Check if value exists |
| `set.delete(value)` | Remove a value (returns boolean) |
| `set.clear()` | Remove all values |
| `set.size` | Number of values |
| `set.forEach(callback)` | Iterate over values |
| `set.values()` | Returns an iterator over values |
| `set.keys()` | Returns an iterator over values (same as `values()` for Set) |
| `set.entries()` | Returns an iterator of `[value, value]` pairs (matches `Map.entries()` shape) |
| `set[Symbol.iterator]()` | Returns a values iterator (same as `values()`) |
| `set.union(other)` | Returns new Set with elements from both |
| `set.intersection(other)` | Returns new Set with common elements |
| `set.difference(other)` | Returns new Set with elements in this but not other |
| `set.symmetricDifference(other)` | Returns new Set with elements in either but not both |
| `set.isSubsetOf(other)` | Check if all elements are in other |
| `set.isSupersetOf(other)` | Check if contains all elements of other |
| `set.isDisjointFrom(other)` | Check if no common elements |

Sets are iterable: `[...mySet]` spreads the set's values into an array.

### Map (`Goccia.Builtins.GlobalMap.pas`)

A collection of key-value pairs with insertion-order iteration. Any value (including objects) can be a key.

| Method/Property | Description |
|--------|-------------|
| `new Map(entries?)` | Create a new Map, optionally from `[[key, value], ...]` |
| `map.get(key)` | Get value for key |
| `map.set(key, value)` | Set a key-value pair (returns the Map for chaining) |
| `map.has(key)` | Check if key exists |
| `map.delete(key)` | Remove a key (returns boolean) |
| `map.clear()` | Remove all entries |
| `map.size` | Number of entries |
| `map.forEach(callback)` | Iterate over entries |
| `map.keys()` | Returns an iterator over keys |
| `map.values()` | Returns an iterator over values |
| `map.entries()` | Returns an iterator over `[key, value]` pairs |
| `map[Symbol.iterator]()` | Returns an entries iterator (same as `entries()`) |
| `map.getOrInsert(key, default)` | Return value for key if present; otherwise insert default and return it (TC39 proposal-upsert) |
| `map.getOrInsertComputed(key, cb)` | Return value for key if present; otherwise call `cb(key)`, insert result, and return it (TC39 proposal-upsert) |

**Static methods:**

| Method | Description |
|--------|-------------|
| `Map.groupBy(iterable, callback)` | Group elements into a Map by callback return value |

Maps are iterable: `[...myMap]` spreads the map into an array of `[key, value]` pairs.

### Promise (`Goccia.Builtins.GlobalPromise.pas`, `Goccia.Values.PromiseValue.pas`)

An implementation of ECMAScript Promises with a synchronous microtask queue. `.then()` callbacks are always deferred (never synchronous), matching spec behavior.

**Constructor:**

```javascript
const p = new Promise((resolve, reject) => {
  // executor runs synchronously
  resolve(42);
});
```

**Prototype methods:**

| Method | Description |
|--------|-------------|
| `promise.then(onFulfilled?, onRejected?)` | Attach fulfillment/rejection handlers. Returns a new Promise. |
| `promise.catch(onRejected)` | Sugar for `.then(undefined, onRejected)` |
| `promise.finally(onFinally)` | Runs callback regardless of outcome, preserves settlement value |

**Static methods:**

| Method | Description |
|--------|-------------|
| `Promise.resolve(value)` | Create a fulfilled Promise (returns `value` if already a Promise) |
| `Promise.reject(reason)` | Create a rejected Promise |
| `Promise.all(iterable)` | Resolve when all resolve; reject on first rejection |
| `Promise.allSettled(iterable)` | Wait for all to settle; returns `{status, value/reason}` objects |
| `Promise.race(iterable)` | Settle with first settled value |
| `Promise.any(iterable)` | Resolve with first fulfillment; reject with AggregateError if all reject |
| `Promise.withResolvers()` | Returns object with `promise`, `resolve`, `reject` |
| `Promise.try(callback)` | Execute callback, wrap result or error in Promise |

**Microtask queue:** `.then()` callbacks are enqueued as microtasks and drained automatically after script execution completes — the script is one macrotask, and microtasks drain after it finishes, matching ECMAScript specification behavior. Thenable adoption (resolving a Promise with another Promise) is deferred via a microtask per the spec's PromiseResolveThenableJob, ensuring correct ordering relative to other microtasks. If the script throws an exception, pending microtasks are discarded via `ClearQueue` in a `finally` block, preventing stale callbacks from leaking into subsequent executions. The test framework also drains microtasks after each test callback, so tests can return a Promise and place assertions inside `.then()` handlers. The benchmark runner drains after each measurement round.

**Async test pattern:**

```javascript
test("async test", () => {
  return Promise.resolve(42).then((v) => {
    expect(v).toBe(42);
  });
});
```

If a test returns a rejected Promise, the test framework automatically fails the test with the rejection reason.

**Thenable adoption:** Resolving a Promise with another Promise causes it to adopt the inner Promise's state. Resolving a Promise with itself throws a `TypeError` ("Chaining cycle detected for promise"), matching ECMAScript spec behavior.

**Error validation:** Static combinator methods (`Promise.all`, `Promise.allSettled`, `Promise.race`, `Promise.any`) accept any iterable argument (arrays, strings, Sets, Maps). Non-iterable arguments (numbers, booleans, null, undefined, plain objects) cause the returned Promise to reject with a `TypeError`, matching ECMAScript spec behavior where the rejection is asynchronous rather than a synchronous throw.

**Chaining and error recovery:**

```javascript
Promise.resolve(1)
  .then((v) => v + 1)        // 2
  .then((v) => { throw "err"; })
  .catch((e) => "recovered") // "recovered"
  .then((v) => v);           // "recovered"
```

### Performance (`Goccia.Builtins.Performance.pas`)

`performance` is exposed as a global `Performance` instance, not as a plain namespace object. The global `Performance` interface object is also exposed, with `Performance.prototype === Object.getPrototypeOf(performance)`. `now()`, `toJSON()`, `timeOrigin`, and `Symbol.toStringTag` live on the shared prototype.

Core High Resolution Time API:

| Member | Description |
|--------|-------------|
| `performance.now()` | Monotonic elapsed time in milliseconds since the engine's time origin |
| `performance.timeOrigin` | Unix epoch timestamp in milliseconds for the engine's time origin |
| `performance.toJSON()` | Returns `{ timeOrigin }` |
| `performance[Symbol.toStringTag]` | `"Performance"` |

`performance.now()` uses `TimingUtils.GetNanoseconds`, so wall-clock changes do not affect it. `performance.timeOrigin` is captured once from `TimingUtils.GetEpochNanoseconds` when the built-in is created.

`Performance()` and `new Performance()` both throw `TypeError` (`"Illegal constructor"`), matching the web platform's non-constructible interface object behavior.

### Temporal (`Goccia.Builtins.Temporal.pas`)

An implementation of the ECMAScript Temporal API (Stage 3 proposal) providing modern date/time handling. ISO 8601 calendar only. All Temporal types are immutable — operations return new instances.

**Namespace structure:**

```javascript
Temporal.Now           // Current time utilities
Temporal.Duration      // Time duration representation
Temporal.Instant       // Absolute point in time (epoch-based)
Temporal.PlainDate     // Calendar date (no time/timezone)
Temporal.PlainTime     // Wall-clock time (no date/timezone)
Temporal.PlainDateTime // Date + time (no timezone)
Temporal.PlainYearMonth // Year and month (no day/time/timezone)
Temporal.PlainMonthDay  // Month and day (no year/time/timezone)
Temporal.ZonedDateTime  // Date + time + timezone
```

#### Temporal.Duration

Represents a length of time with 10 components (years through nanoseconds).

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.Duration(y?, mo?, w?, d?, h?, min?, s?, ms?, us?, ns?)` | Create from components (all default to 0) |
| `Temporal.Duration.from(item)` | Create from string (`"P1Y2M3DT4H5M6S"`), Duration, or object |
| `Temporal.Duration.compare(one, two)` | Compare two durations (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `years`, `months`, `weeks`, `days` | Date components |
| `hours`, `minutes`, `seconds` | Time components |
| `milliseconds`, `microseconds`, `nanoseconds` | Sub-second components |
| `sign` | -1, 0, or 1 |
| `blank` | True if all components are zero |

| Method | Description |
|--------|-------------|
| `negated()` | Return negated duration |
| `abs()` | Return absolute duration |
| `add(other)` | Add another duration |
| `subtract(other)` | Subtract another duration |
| `with(fields)` | Return new duration with overridden fields |
| `total(unit)` | Convert to total of a single unit (e.g., `"hours"`). Accepts a string or options object `{ unit, relativeTo? }`. Throws `RangeError` if duration has non-zero years/months without `relativeTo`. Calendar-relative conversion (`relativeTo`) is not yet supported. |
| `round(options)` | Round the duration. Accepts a string (smallestUnit) or options object `{ smallestUnit, largestUnit, roundingMode, roundingIncrement }`. Rebalances components from `largestUnit` down. Throws `RangeError` if duration has years/months (requires `relativeTo`, not yet supported). At least `smallestUnit` or `largestUnit` must be specified. |
| `toString()` / `toJSON()` | ISO 8601 duration string (e.g., `"P1Y2M3DT4H5M6S"`) |
| `valueOf()` | Throws TypeError (prevents implicit coercion) |

#### Temporal.PlainDate

Represents a calendar date without time or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainDate(year, month, day)` | Create from components |
| `Temporal.PlainDate.from(item [, options])` | Create from string (`"2024-03-15"`), PlainDate, or object. Options: `{ overflow }` where overflow is `"constrain"` (default, clamps out-of-range values) or `"reject"` (throws RangeError). |
| `Temporal.PlainDate.compare(one, two)` | Compare two dates (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `year`, `month`, `day` | Date components |
| `monthCode` | `"M01"` through `"M12"` |
| `dayOfWeek` | 1 (Monday) through 7 (Sunday) |
| `dayOfYear`, `weekOfYear`, `yearOfWeek` | ISO week-date components |
| `daysInWeek`, `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new date with overridden fields |
| `add(duration)` / `subtract(duration)` | Date arithmetic |
| `until(other)` / `since(other)` | Difference as Duration |
| `equals(other)` | Equality check |
| `toPlainDateTime(time?)` | Combine with a time |
| `toPlainYearMonth()` | Extract year and month as PlainYearMonth |
| `toPlainMonthDay()` | Extract month and day as PlainMonthDay |
| `toZonedDateTime(timeZone)` | Combine with a timezone (string or `{ timeZone }` object) to create a ZonedDateTime at midnight |
| `toString()` / `toJSON()` | ISO date string (e.g., `"2024-03-15"`) |
| `valueOf()` | Throws TypeError |

#### Temporal.PlainTime

Represents a wall-clock time without date or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainTime(h?, min?, s?, ms?, us?, ns?)` | Create from components (all default to 0) |
| `Temporal.PlainTime.from(item)` | Create from string (`"13:45:30"`), PlainTime, or object |
| `Temporal.PlainTime.compare(one, two)` | Compare two times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `hour`, `minute`, `second` | Time components |
| `millisecond`, `microsecond`, `nanosecond` | Sub-second components |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new time with overridden fields |
| `add(duration)` / `subtract(duration)` | Time arithmetic (wraps at midnight) |
| `until(other)` / `since(other)` | Difference as Duration |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toString([options])` / `toJSON()` | ISO time string (e.g., `"13:45:30"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

#### Temporal.PlainDateTime

Represents a date and time without timezone. Combines PlainDate and PlainTime.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainDateTime(y, mo, d, h?, min?, s?, ms?, us?, ns?)` | Create from components |
| `Temporal.PlainDateTime.from(item)` | Create from string, PlainDateTime, or object |
| `Temporal.PlainDateTime.compare(one, two)` | Compare two date-times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| All PlainDate getters + all PlainTime getters | Combined date and time access |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new date-time with overridden fields |
| `withPlainTime(time?)` | Replace time component |
| `add(duration)` / `subtract(duration)` | Date-time arithmetic |
| `until(other)` / `since(other)` | Difference as Duration |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toPlainDate()` / `toPlainTime()` | Extract date or time component |
| `toPlainYearMonth()` | Extract year and month as PlainYearMonth |
| `toPlainMonthDay()` | Extract month and day as PlainMonthDay |
| `toZonedDateTime(timeZone)` | Combine with a timezone (string or `{ timeZone }` object) to create a ZonedDateTime |
| `toString([options])` / `toJSON()` | ISO string (e.g., `"2024-03-15T13:45:30"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

#### Temporal.Instant

Represents an absolute point in time (epoch-based), independent of calendar or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.Instant(epochNanoseconds)` | Create from epoch nanoseconds |
| `Temporal.Instant.from(item)` | Create from string or Instant |
| `Temporal.Instant.fromEpochMilliseconds(ms)` | Create from epoch milliseconds |
| `Temporal.Instant.fromEpochNanoseconds(ns)` | Create from epoch nanoseconds |
| `Temporal.Instant.compare(one, two)` | Compare two instants (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `epochMilliseconds` | Milliseconds since Unix epoch |
| `epochNanoseconds` | Nanoseconds since Unix epoch (as number) |

| Method | Description |
|--------|-------------|
| `add(duration)` / `subtract(duration)` | Time arithmetic (no calendar units) |
| `until(other)` / `since(other)` | Difference as Duration |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toString([options])` / `toJSON()` | ISO string with UTC (e.g., `"2024-03-15T13:45:30Z"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

#### Temporal.PlainYearMonth

Represents a year and month without a day, time, or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainYearMonth(year, month)` | Create from components |
| `Temporal.PlainYearMonth.from(item)` | Create from string (`"2024-03"`), PlainYearMonth, or object |
| `Temporal.PlainYearMonth.compare(one, two)` | Compare two year-months (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `year`, `month` | Date components |
| `monthCode` | `"M01"` through `"M12"` |
| `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new year-month with overridden fields |
| `add(duration)` / `subtract(duration)` | Year-month arithmetic (years and months only) |
| `until(other)` / `since(other)` | Difference as Duration (years and months) |
| `equals(other)` | Equality check |
| `toPlainDate(item)` | Combine with a day (`{ day }`) to create a PlainDate |
| `toString()` / `toJSON()` | ISO string (e.g., `"2024-03"`) |
| `valueOf()` | Throws TypeError |

#### Temporal.PlainMonthDay

Represents a month and day without a year, time, or timezone. Uses a reference year (1972) for validation.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainMonthDay(month, day)` | Create from components |
| `Temporal.PlainMonthDay.from(item)` | Create from string (`"12-25"`), PlainMonthDay, or object with `{ monthCode, day }` |
| `Temporal.PlainMonthDay.compare(one, two)` | Compare two month-days (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `monthCode` | `"M01"` through `"M12"` |
| `day` | Day of month |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new month-day with overridden fields (accepts `monthCode` and `day`) |
| `equals(other)` | Equality check |
| `toPlainDate(item)` | Combine with a year (`{ year }`) to create a PlainDate |
| `toString()` / `toJSON()` | Month-day string (e.g., `"12-25"`) |
| `valueOf()` | Throws TypeError |

#### Temporal.ZonedDateTime

Represents an absolute date and time in a specific timezone. Combines an instant (epoch-based) with a timezone identifier for wall-clock interpretation.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.ZonedDateTime(epochNanoseconds, timeZone)` | Create from epoch nanoseconds and timezone ID |
| `Temporal.ZonedDateTime.from(item)` | Create from ISO string with timezone annotation (e.g., `"2024-03-15T13:45:30+05:30[Asia/Kolkata]"`), ZonedDateTime, or object |
| `Temporal.ZonedDateTime.compare(one, two)` | Compare two zoned date-times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `timeZoneId` | IANA timezone identifier (e.g., `"America/New_York"`) |
| `year`, `month`, `monthCode`, `day` | Date components (wall-clock, timezone-adjusted) |
| `dayOfWeek`, `dayOfYear`, `weekOfYear`, `yearOfWeek` | ISO week-date components |
| `daysInWeek`, `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |
| `hoursInDay` | Hours in the wall-clock day (accounts for DST transitions) |
| `hour`, `minute`, `second` | Time components |
| `millisecond`, `microsecond`, `nanosecond` | Sub-second components |
| `offset` | UTC offset string (e.g., `"+05:30"`) |
| `offsetNanoseconds` | UTC offset in nanoseconds |
| `epochMilliseconds` | Milliseconds since Unix epoch |
| `epochNanoseconds` | Nanoseconds since Unix epoch (as number) |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new ZonedDateTime with overridden fields |
| `withPlainDate(date)` | Replace date component |
| `withPlainTime(time?)` | Replace time component |
| `withTimeZone(timeZone)` | Re-interpret the same instant in a different timezone |
| `add(duration)` / `subtract(duration)` | Date-time arithmetic |
| `until(other)` / `since(other)` | Difference as Duration |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `startOfDay()` | Return ZonedDateTime at the start of the wall-clock day |
| `toInstant()` | Extract the underlying Instant |
| `toPlainDate()` / `toPlainTime()` | Extract date or time component |
| `toPlainDateTime()` | Extract date-time without timezone |
| `toString([options])` / `toJSON()` | ISO string with offset and timezone annotation (e.g., `"2024-03-15T13:45:30+05:30[Asia/Kolkata]"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

#### Temporal.Now

Provides current time in various representations.

| Method | Description |
|--------|-------------|
| `Temporal.Now.timeZoneId()` | System timezone identifier (e.g., `"America/New_York"`) |
| `Temporal.Now.instant()` | Current time as Instant |
| `Temporal.Now.zonedDateTimeISO([timeZone])` | Current date-time as ZonedDateTime in the given timezone (defaults to system timezone) |
| `Temporal.Now.plainDateISO()` | Current date as PlainDate |
| `Temporal.Now.plainTimeISO()` | Current time as PlainTime |
| `Temporal.Now.plainDateTimeISO()` | Current date-time as PlainDateTime |

### ArrayBuffer (`Goccia.Builtins.GlobalArrayBuffer.pas`, `Goccia.Values.ArrayBufferValue.pas`)

A raw binary data buffer. Supports both fixed-length and resizable modes. Internally backed by a zero-initialized `TBytes` array. Resizable buffers are created by passing `{ maxByteLength }` in the options argument. Buffers can be transferred (moving ownership to a new buffer and detaching the original).

**Constructor:**

| Constructor | Description |
|-------------|-------------|
| `new ArrayBuffer(length)` | Create a fixed-length buffer with the given byte length (must be a non-negative integer). Throws `RangeError` for negative, fractional, NaN, or Infinity values. |
| `new ArrayBuffer(length, { maxByteLength })` | Create a resizable buffer with the given initial byte length and maximum capacity. Throws `RangeError` if `length > maxByteLength`. |

**Static methods:**

| Method | Description |
|--------|-------------|
| `ArrayBuffer.isView(arg)` | Returns `true` if `arg` is a TypedArray, `false` otherwise |

**Prototype properties:**

| Property | Description |
|----------|-------------|
| `buf.byteLength` | The current size in bytes. Returns 0 for detached buffers. |
| `buf.maxByteLength` | Maximum byte length for resizable buffers; equals `byteLength` for fixed-length buffers. Returns 0 for detached buffers. |
| `buf.resizable` | `true` if the buffer was created with `maxByteLength`, `false` otherwise. Preserved after detachment. |
| `buf.detached` | `true` if the buffer has been detached (via `transfer` or `transferToFixedLength`), `false` otherwise. |
| `buf[Symbol.toStringTag]` | `"ArrayBuffer"` |

**Prototype methods:**

| Method | Description |
|--------|-------------|
| `buf.slice(begin?, end?)` | Returns a new ArrayBuffer containing bytes from `begin` (inclusive) to `end` (exclusive). Supports negative indices (count from end). Clamps out-of-range indices. Defaults: `begin` = 0, `end` = `byteLength`. Throws `TypeError` if detached. |
| `buf.resize(newLength)` | Resizes a resizable buffer to `newLength` bytes. Preserves existing data up to `min(old, new)` length; zero-fills growth. Throws `TypeError` if fixed-length or detached. Throws `RangeError` if `newLength > maxByteLength`. Returns `undefined`. |
| `buf.transfer([newLength])` | Copies data into a new buffer and detaches the original. `newLength` defaults to current `byteLength`. Preserves resizability: if the source was resizable, the new buffer keeps the original `maxByteLength`. Throws `RangeError` if `newLength` exceeds that cap, and `TypeError` if detached. |
| `buf.transferToFixedLength([newLength])` | Like `transfer`, but always produces a fixed-length (non-resizable) result. `newLength` defaults to current `byteLength`. Throws `TypeError` if detached. |

**structuredClone:** ArrayBuffer instances are cloneable. The byte contents are copied into a new buffer.

### SharedArrayBuffer (`Goccia.Values.SharedArrayBufferValue.pas`)

A fixed-length raw binary data buffer intended for shared-memory use. In GocciaScript, `SharedArrayBuffer` has the same API as `ArrayBuffer` but is a distinct type (not an instance of `ArrayBuffer`).

**Constructor:**

| Constructor | Description |
|-------------|-------------|
| `new SharedArrayBuffer(length)` | Create a buffer with the given byte length (must be a non-negative integer). Throws `RangeError` for negative, fractional, NaN, or Infinity values. |

**Prototype properties:**

| Property | Description |
|----------|-------------|
| `sab.byteLength` | The size, in bytes, of the SharedArrayBuffer (read-only getter) |
| `sab[Symbol.toStringTag]` | `"SharedArrayBuffer"` |

**Prototype methods:**

| Method | Description |
|--------|-------------|
| `sab.slice(begin?, end?)` | Returns a new SharedArrayBuffer containing bytes from `begin` (inclusive) to `end` (exclusive). Supports negative indices. Clamps out-of-range indices. |

**structuredClone:** SharedArrayBuffer instances are cloneable. The byte contents are copied into a new buffer.

### TypedArrays (`Goccia.Values.TypedArrayValue.pas`)

TypedArrays provide array-like views over ArrayBuffer data with fixed element types. GocciaScript supports 10 non-BigInt TypedArray types:

| Type | Element size | Value range |
|------|-------------|-------------|
| `Int8Array` | 1 byte | -128 to 127 |
| `Uint8Array` | 1 byte | 0 to 255 |
| `Uint8ClampedArray` | 1 byte | 0 to 255 (clamped) |
| `Int16Array` | 2 bytes | -32768 to 32767 |
| `Uint16Array` | 2 bytes | 0 to 65535 |
| `Int32Array` | 4 bytes | -2147483648 to 2147483647 |
| `Uint32Array` | 4 bytes | 0 to 4294967295 |
| `Float16Array` | 2 bytes | IEEE 754 half-precision |
| `Float32Array` | 4 bytes | IEEE 754 single-precision |
| `Float64Array` | 8 bytes | IEEE 754 double-precision |

**Constructors:**

| Constructor | Description |
|-------------|-------------|
| `new TypedArray()` | Zero-length typed array with its own buffer |
| `new TypedArray(length)` | Creates array of `length` elements, zero-initialized |
| `new TypedArray(typedArray)` | Copies elements from another typed array (with type conversion) |
| `new TypedArray(array)` | Creates from a regular array |
| `new TypedArray(buffer [, byteOffset [, length]])` | Creates a view over an ArrayBuffer or SharedArrayBuffer. The `.buffer` property returns the original buffer object. |

**Static properties:**

| Property | Description |
|----------|-------------|
| `TypedArray.BYTES_PER_ELEMENT` | Element size in bytes |

**Static methods:**

| Method | Description |
|--------|-------------|
| `TypedArray.from(source [, mapFn])` | Creates from array or typed array, with optional mapping |
| `TypedArray.of(...items)` | Creates from argument list |

**Instance properties (getters):**

| Property | Description |
|----------|-------------|
| `ta.buffer` | The underlying ArrayBuffer |
| `ta.byteLength` | Total size in bytes |
| `ta.byteOffset` | Offset into the buffer |
| `ta.length` | Number of elements |
| `ta.BYTES_PER_ELEMENT` | Element size in bytes |

**Prototype methods:**

| Method | Description |
|--------|-------------|
| `ta.at(index)` | Element at index (supports negative) |
| `ta.fill(value [, start [, end]])` | Fill range with value |
| `ta.copyWithin(target, start [, end])` | Copy elements within array |
| `ta.slice(start?, end?)` | New typed array from slice |
| `ta.subarray(begin?, end?)` | New view sharing the same buffer |
| `ta.set(source [, offset])` | Copy from array/typed array into this |
| `ta.reverse()` | Reverse in place |
| `ta.sort([compareFn])` | Sort in place (numeric default) |
| `ta.indexOf(value [, fromIndex])` | First index of value |
| `ta.lastIndexOf(value [, fromIndex])` | Last index of value |
| `ta.includes(value [, fromIndex])` | Whether value exists (SameValueZero) |
| `ta.find(predicate)` | First element matching predicate |
| `ta.findIndex(predicate)` | First index matching predicate |
| `ta.findLast(predicate)` | Last element matching predicate |
| `ta.findLastIndex(predicate)` | Last index matching predicate |
| `ta.every(predicate)` | Whether all elements match |
| `ta.some(predicate)` | Whether any element matches |
| `ta.forEach(callback)` | Call function for each element |
| `ta.map(callback)` | New typed array from mapping |
| `ta.filter(predicate)` | New typed array from filter |
| `ta.reduce(callback [, initialValue])` | Reduce left-to-right |
| `ta.reduceRight(callback [, initialValue])` | Reduce right-to-left |
| `ta.join(separator?)` | Join elements as string |
| `ta.toString()` | Same as `join()` |
| `ta.toReversed()` | New reversed copy |
| `ta.toSorted([compareFn])` | New sorted copy |
| `ta.with(index, value)` | New copy with one element replaced |
| `ta.values()` | Iterator over values |
| `ta.keys()` | Iterator over indices |
| `ta.entries()` | Iterator over [index, value] pairs |
| `ta[Symbol.iterator]()` | Same as `values()` |

**Uint8Array Base64/Hex encoding** (TC39 Uint8Array Base64, `Goccia.Values.Uint8ArrayEncoding.pas`):

These methods are available only on `Uint8Array`, not on other TypedArray types.

| Static method | Description |
|--------|-------------|
| `Uint8Array.fromBase64(string [, options])` | Decode a base64 string to a new Uint8Array. Options: `alphabet` (`"base64"` or `"base64url"`), `lastChunkHandling` (`"loose"`, `"strict"`, or `"stop-before-partial"`) |
| `Uint8Array.fromHex(string)` | Decode a hex string (case-insensitive) to a new Uint8Array. Throws `SyntaxError` on odd length or invalid characters |

| Prototype method | Description |
|--------|-------------|
| `u8.toBase64([options])` | Encode bytes as a base64 string. Options: `alphabet` (`"base64"` or `"base64url"`), `omitPadding` (boolean, default `false`) |
| `u8.toHex()` | Encode bytes as a lowercase hex string |
| `u8.setFromBase64(string [, options])` | Decode base64 into this array. Returns `{ read, written }`. Same options as `fromBase64` |
| `u8.setFromHex(string)` | Decode hex into this array. Returns `{ read, written }` |

**Value encoding:** Integer types use fixed-width truncation (overflow wraps). `Uint8ClampedArray` clamps to [0, 255] with half-to-even rounding. `Float16Array` rounds to IEEE 754 half precision (max finite ±65504, epsilon 2⁻¹⁰ at 1.0). `Float32Array` rounds to IEEE 754 single precision. `Float64Array` preserves full double precision. NaN is stored as 0 in integer types and as NaN in float types.

**Not supported:** `BigInt64Array`, `BigUint64Array` (BigInt types), `DataView`.

### FFI (`Goccia.Builtins.GlobalFFI.pas`)

Foreign Function Interface for calling native shared libraries. Only available when `ggFFI` is enabled (not registered by default).

**FFI global object:**

| Method/Property | Description |
|--------|-------------|
| `FFI.open(path)` | Open a dynamic library, returns an `FFILibrary` |
| `FFI.nullptr` | Singleton null pointer value |
| `FFI.suffix` | Platform library suffix (`.dylib`, `.so`, `.dll`) |

**FFILibrary methods:**

| Method/Property | Description |
|--------|-------------|
| `library.bind(funcName, signature)` | Bind a native function. `signature` is `{ args: ['i32', ...], returns: 'i32' }`. Returns a callable function. |
| `library.symbol(name)` | Get raw pointer to a named symbol |
| `library.close()` | Unload the library |
| `library.path` | Full path to the loaded library |
| `library.closed` | Whether the library has been closed |

**FFIPointer properties:**

| Property | Description |
|----------|-------------|
| `ptr.address` | Numeric address |
| `ptr.isNull` | Whether pointer is null |

**Supported FFI types:** `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `pointer`, `cstring`, `bool`, `void` (return only). `i64`/`u64` are not available on i386. Max 8 arguments per function. Pointer arguments accept `FFIPointer`, `ArrayBuffer`, `SharedArrayBuffer`, `TypedArray`, or `null`.

### Test Assertions (`Goccia.Builtins.TestAssertions.pas`)

Only available when `ggTestAssertions` is enabled.

**Test structure:**

```javascript
describe("group name", () => {
  test("test name", () => {
    expect(value).toBe(expected);
  });
});
```

**Functions:** `describe`, `describe.skip`, `describe.skipIf`, `describe.runIf`, `describe.only`, `describe.each`, `test`, `it` (alias for `test`), `test.skip`, `test.skipIf`, `test.runIf`, `test.only`, `it.only`, `test.each`, `test.todo`, `beforeAll`, `beforeEach`, `afterEach`, `afterAll`

**Vitest-style structure helpers:**

- `beforeAll(fn)` / `afterAll(fn)` run once per suite around that suite's tests
- `beforeEach(fn)` / `afterEach(fn)` are suite-scoped and inherited by nested suites
- `test.each(table)(name, fn)` expands one test per table row and passes row values as callback arguments
- `describe.each(table)(name, fn)` expands one suite per table row and passes row values as callback arguments
- `test.only(...)`, `it.only(...)`, and `describe.only(...)` focus execution to the selected tests/suites
- `test.todo(name)` registers a placeholder test that is reported as skipped but never executed

**Matchers:**

| Matcher | Description |
|---------|-------------|
| `.toBe(expected)` | Strict equality (`===`) |
| `.toEqual(expected)` | Deep equality |
| `.toStrictEqual(expected)` | Deep equality alias for Vitest compatibility |
| `.toBeNull()` | Is null |
| `.toBeNaN()` | Is NaN |
| `.toBeUndefined()` | Is undefined |
| `.toBeDefined()` | Is not undefined |
| `.toBeTruthy()` | Truthy value |
| `.toBeFalsy()` | Falsy value |
| `.toBeGreaterThan(n)` | Greater than |
| `.toBeLessThan(n)` | Less than |
| `.toContain(item)` | Array/Set element or string substring |
| `.toContainEqual(item)` | Deep-equal array element |
| `.toMatch(stringOrRegExp)` | String substring or regular expression match |
| `.toMatchObject(obj)` | Partial recursive object/subset match |
| `.toBeInstanceOf(class)` | instanceof check |
| `.toHaveLength(n)` | Length check |
| `.toHaveProperty(name)` | Property exists on an object. Non-object values always fail; negated (`not.toHaveProperty`) on non-objects passes. |
| `.toThrow(ErrorType?)` | Throws an error (optionally checks error constructor) |
| `.toBeCloseTo(n, digits?)` | Approximate equality |

All matchers support `.not` negation: `expect(value).not.toBe(wrong)`.

`.toMatch()` accepts either a string substring or a `RegExp` object. Regex matches ignore the regex's current `lastIndex`, matching Jest/Vitest-style assertion ergonomics.

### Benchmark (`Goccia.Builtins.Benchmark.pas`)

Only available when `ggBenchmark` is enabled (used by the BenchmarkRunner). See [benchmarks.md](benchmarks.md) for usage, output formats, and CI integration.

**Benchmark structure:**

```javascript
suite("group name", () => {
  bench("benchmark name", () => {
    // Code to benchmark — called many times during measurement
    someOperation();
  });
});
```

**Functions:**

| Function | Description |
|----------|-------------|
| `suite(name, fn)` | Group benchmarks. Executes `fn` immediately to register `bench` entries. |
| `bench(name, fn)` | Register a benchmark function. Called repeatedly during calibration and measurement. |
| `runBenchmarks()` | Execute all registered benchmarks and return results. Injected automatically by BenchmarkRunner. |

**Result object** (returned by `runBenchmarks()`):

Each benchmark result includes: `name`, `suite`, `opsPerSec`, `meanMs`, `iterations`, `totalMs`, `variancePercentage`, and optionally `error`.

### Proxy (`Goccia.Builtins.GlobalProxy.pas`, `Goccia.Values.ProxyValue.pas`)

ES2026 Proxy constructor with all 13 handler traps and invariant enforcement.

**Constructor:**

| Constructor | Description |
|-------------|-------------|
| `new Proxy(target, handler)` | Create a proxy wrapping `target` with `handler` traps. Both must be objects; throws `TypeError` otherwise. |

**Static methods:**

| Method | Description |
|--------|-------------|
| `Proxy.revocable(target, handler)` | Returns `{ proxy, revoke }`. Calling `revoke()` causes all subsequent trap operations to throw `TypeError`. |

**Supported traps:**

| Trap | Handler Signature | Triggered by |
|------|-------------------|-------------|
| `get` | `(target, prop, receiver)` | Property read (`proxy.x`, `proxy[key]`) |
| `set` | `(target, prop, value, receiver) → boolean` | Property write (`proxy.x = v`) |
| `has` | `(target, prop) → boolean` | `key in proxy` |
| `deleteProperty` | `(target, prop) → boolean` | `delete proxy.x` |
| `getOwnPropertyDescriptor` | `(target, prop) → descriptor \| undefined` | `Object.getOwnPropertyDescriptor(proxy, key)` |
| `defineProperty` | `(target, prop, descriptor) → boolean` | `Object.defineProperty(proxy, key, desc)` |
| `getPrototypeOf` | `(target) → object \| null` | `Object.getPrototypeOf(proxy)` |
| `setPrototypeOf` | `(target, proto) → boolean` | `Object.setPrototypeOf(proxy, proto)` |
| `isExtensible` | `(target) → boolean` | `Object.isExtensible(proxy)` |
| `preventExtensions` | `(target) → boolean` | `Object.preventExtensions(proxy)` |
| `ownKeys` | `(target) → array` | `Object.keys(proxy)`, `Reflect.ownKeys(proxy)` |
| `apply` | `(target, thisArg, args)` | Function call `proxy(...)` (callable proxies only) |
| `construct` | `(target, args, newTarget) → object` | `new proxy(...)` (callable proxies only) |

All traps support both string and symbol property keys. Each trap enforces ES2026 invariants (e.g., `get` on a non-configurable non-writable property must return the same value).

### Reflect (`Goccia.Builtins.GlobalReflect.pas`)

The `Reflect` object provides methods for interceptable JavaScript operations. Unlike `Object.*` methods which throw on failure, `Reflect` methods that operate on property descriptors return boolean results. All `Reflect` methods throw `TypeError` when the target is not an object.

| Method | Description |
|--------|-------------|
| `Reflect.apply(target, thisArg, args)` | Calls a function with the given `this` value and arguments array |
| `Reflect.construct(target, args [, newTarget])` | Creates an instance like `new target(...args)`, optionally with a different `newTarget` prototype |
| `Reflect.defineProperty(target, key, attrs)` | Like `Object.defineProperty` but returns `true`/`false` instead of throwing |
| `Reflect.deleteProperty(target, key)` | Deletes a property, returns `true`/`false` |
| `Reflect.get(target, key [, receiver])` | Gets a property value. Supports symbol keys. |
| `Reflect.getOwnPropertyDescriptor(target, key)` | Returns an own property descriptor or `undefined` |
| `Reflect.getPrototypeOf(target)` | Returns the prototype of the target |
| `Reflect.has(target, key)` | Like the `in` operator — checks property existence on the prototype chain |
| `Reflect.isExtensible(target)` | Returns whether the target is extensible |
| `Reflect.ownKeys(target)` | Returns an array of own string keys followed by own symbol keys |
| `Reflect.preventExtensions(target)` | Prevents new properties, returns `true` |
| `Reflect.set(target, key, value [, receiver])` | Sets a property value. Supports symbol keys. Returns `true`/`false`. |
| `Reflect.setPrototypeOf(target, proto)` | Sets the prototype. Returns `false` for non-extensible objects (instead of throwing). |
