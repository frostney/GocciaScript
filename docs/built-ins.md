# Built-in Objects

*For contributors adding or modifying built-in objects, and for script authors looking up available APIs.*

## Executive Summary

- **Unconditional registration** — Standard built-ins (Console, Math, Object, Array, String, Number, RegExp, JSON, JSON5, JSONL, TOML, YAML, CSV, TSV, Symbol, Set, Map, Promise, Performance, Temporal, ArrayBuffer, SharedArrayBuffer, TypedArrays, Proxy, Reflect, Iterator, TextEncoder, TextDecoder, URL, URLSearchParams, fetch, Headers, Response, DisposableStack, AsyncDisposableStack) are always registered
- **Flag-gated extras** — Only `ggTestAssertions`, `ggBenchmark`, and `ggFFI` use flag-gating for opt-in registration
- **Adding new built-ins** — See [Adding Built-in Types](adding-built-in-types.md) for the step-by-step recipe
- **Always-present globals** — `globalThis` and `Goccia` namespace are registered after all built-ins

GocciaScript provides a set of built-in global objects that mirror JavaScript's standard library. Each built-in is implemented as a Pascal unit and registered unconditionally by the engine. Only test assertions, benchmarks, and FFI use flag-gated registration.

## Registration System

Standard built-ins (Console, Math, Object, Array, Number, JSON, JSON5, JSONL, TOML, YAML, CSV, TSV, Symbol, Set, Map, Promise, Performance, Temporal, ArrayBuffer, Proxy, Reflect, fetch, Headers, Response) are always registered unconditionally by the engine. There is no flag-gating for these — they are available in every execution context.

Only three built-ins use flag-gated registration via the `TGocciaGlobalBuiltins` enum:

```pascal
TGocciaGlobalBuiltin = (
  ggTestAssertions,   // describe, test, expect (testing only)
  ggBenchmark,        // suite, bench, runBenchmarks (benchmarking only)
  ggFFI              // Foreign Function Interface
);
```

The `GocciaTestRunner` adds `ggTestAssertions` to inject the test framework.
The `GocciaBenchmarkRunner` adds `ggBenchmark` to inject the benchmark framework.
FFI (`ggFFI`) requires the `--unsafe-ffi` CLI flag to enable.

## Adding a New Built-in

See [Adding a New Built-in Type](adding-built-in-types.md) for the complete step-by-step guide with code templates, GC considerations, and a checklist.

## Available Built-ins

### Console (`Goccia.Builtins.Console.pas`)

Implements the [WHATWG Console Standard](https://developer.mozilla.org/en-US/docs/Web/API/console). **Not implemented:** `console.dirxml`, `console.groupCollapsed`, `console.profile`, `console.profileEnd`, `console.timeStamp`.

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

#### Output capture

The console supports an `OutputCallback` hook for capturing output programmatically (see [Embedding — Console Output Capture](embedding.md#console-output-capture)).

Separately, the `TGocciaCLIApplication`-based frontends use `LogCallback` for `--log=<file>`, which writes every console call to the specified file in `[method] line` format while preserving normal stdout output.

### Math (`Goccia.Builtins.Math.pas`)

Implements the [ECMAScript Math object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math). See [MDN Math reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math) for the full API.

**Full standard compliance** — all standard constants and methods are available.

**Extras:**

| Method | Description |
|--------|-------------|
| `Math.clamp(x, min, max)` | Clamp to range (Stage 2 [proposal-math-clamp](https://github.com/tc39/proposal-math-clamp)) |
| `Math.f16round(x)` | Nearest 16-bit float |
| `Math.sumPrecise(iterable)` | Precise sum via Kahan-Babuska-Neumaier compensated summation. Throws `TypeError` for non-iterable or non-number elements. Empty iterable returns `-0`. Mixed `±Infinity` returns `NaN`. |

### Data Formats (JSON, JSON5, YAML, JSONL, CSV, TSV, TOML)

See [Data Format Built-ins](built-ins-data-formats.md) for the complete JSON, JSON5, YAML, JSONL, CSV, TSV, and TOML API reference.

### Object (`Goccia.Builtins.GlobalObject.pas`)

Implements the [ECMAScript Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object). See [MDN Object reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object) for the full API.

**Full standard compliance** — all standard static methods and prototype methods are available.

**GocciaScript-specific behavior:** `Object.hasOwn` supports class values and checks static properties (not private fields).

### Array (`Goccia.Builtins.GlobalArray.pas`)

Implements the [ECMAScript Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array). **Not implemented:** `Array.prototype.reduceRight`, `Array.prototype.toLocaleString`.

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

Implements the [ECMAScript Number](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number). See [MDN Number reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number) for the full API.

**Full standard compliance** — all standard static methods, constants, and prototype methods are available.

**GocciaScript difference:** `parseInt`, `parseFloat`, `isNaN`, and `isFinite` are only available as `Number.*` static methods — not as global functions. See [language.md](language.md#no-global-parseint-parsefloat-isnan-isfinite) for the rationale.

### BigInt (`Goccia.Builtins.GlobalBigInt.pas`)

Implements [ECMAScript BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) (ES2020). See [MDN BigInt reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) for the full API.

**Related:** `BigInt64Array` and `BigUint64Array` are implemented — see [Binary Data Built-ins](built-ins-binary-data.md).

### String (`Goccia.Builtins.GlobalString.pas`)

Implements the [ECMAScript String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String). **Not implemented:** `String.prototype.toLocaleLowerCase`, `String.prototype.toLocaleUpperCase`.

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

Implements the [ECMAScript RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp) including modifiers, duplicate named groups, and `RegExp.escape`.

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
| `escape(string)` | Returns a string with all regex-significant characters escaped so the result can be safely interpolated into a pattern. Implements `RegExp.escape`. Syntax characters are backslash-escaped; ClassSet-reserved punctuators, whitespace, and line terminators are hex-encoded (`\xHH` / `\uHHHH`). A leading digit or ASCII letter is also hex-encoded to prevent ambiguity with backreferences. |

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

- **Inline modifier groups** (`(?ims-ims:...)`) are supported per the RegExp Modifiers specification. Only `i` (ignoreCase), `m` (multiline), and `s` (dotAll) are valid modifier flags. The colon form is required — bare `(?ims)` without `:` is a `SyntaxError`. Duplicate flags and flags appearing in both enable and disable sections are rejected. Modifiers are scoped to the group body; outer flags are unaffected.
- **Named capture groups** (`(?<name>...)`) are supported. Match results include a `groups` property (an object with `null` prototype) mapping group names to matched strings. Non-participating named groups are `undefined`. When no named groups exist, `groups` is `undefined`.
- **Duplicate named capture groups:** the same group name may appear in different alternatives of a disjunction (`|`). For example, `/(?<year>\d{4})-\d{2}|\d{2}-(?<year>\d{4})/`. The `groups` property on the match result returns the value from whichever alternative participated; non-participating duplicates are `undefined`. Using the same name in the same alternative (where both groups could participate) is a `SyntaxError`.
- **Named backreferences** (`\k<name>`) reference a previously captured named group within the same pattern. With duplicate named capture groups, `\k<name>` resolves to the group with that name in the same alternative branch.
- Replacement strings in regex-backed `replace()` and `replaceAll()` support `$$`, `$&`, the pre-match token `$` followed by a backtick, `$'`, numeric captures such as `$1`, and **named group references** `$<name>`. An unmatched `$<name>` produces an empty string; `$<` without a closing `>` is literal.
- When the replacer is a function and named groups are present, the `groups` object is passed as the last argument after `input`.
- `String.prototype.match`, `matchAll`, `replace`, `replaceAll`, `search`, and `split` dispatch through the corresponding well-known symbol hooks, so custom protocol objects work as expected.
- `matchAll()` returns a lazy iterator that advances matches on demand per the specification.
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
| `gc` | `function` | Trigger manual garbage collection. Returns `undefined`. Also exposes read-only `gc.bytesAllocated` (approximate GC heap size in bytes) and `gc.maxBytes` (active ceiling; defaults to half of physical memory capped at 8 GB on 64-bit or 700 MB on 32-bit, overridable via `--max-memory`). Allocations exceeding the ceiling throw a `RangeError`. |

**`Goccia.build`**

`Goccia.build` exposes compile-time platform information, mirroring `Deno.build`:

| Property | Type | Description |
|----------|------|-------------|
| `os` | `string` | Operating system: `"darwin"`, `"linux"`, `"windows"`, `"freebsd"`, `"netbsd"`, `"openbsd"`, `"android"`, `"aix"`, `"solaris"`, or `"unknown"` |
| `arch` | `string` | Processor architecture: `"x86_64"`, `"aarch64"`, `"x86"`, `"arm"`, `"powerpc64"`, `"powerpc"`, or `"unknown"` |
| `date` | `string` | Build date in `YYYY-MM-DD` format (e.g., `"2026-04-20"`) |

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

`encodeURI` / `decodeURI` / `encodeURIComponent` / `decodeURIComponent` follow the ECMA-262 URI handling specification. The shared encoding/decoding logic lives in `Goccia.URI.pas` and is also used by `import.meta.url` for file-path percent-encoding. Multi-byte Unicode characters are encoded as UTF-8 octets, each percent-encoded individually (e.g., `encodeURIComponent("中")` → `%E4%B8%AD`). Lone surrogates (U+D800–U+DFFF) throw `URIError`. Decoding validates UTF-8 well-formedness: overlong encodings, truncated sequences, and code points above U+10FFFF all throw `URIError`. `decodeURI` re-emits reserved characters as uppercase percent-encoded sequences even when the input uses lowercase hex digits (e.g., `%2f` → `%2F`).

**Error constructors:** `Error`, `TypeError`, `ReferenceError`, `RangeError`, `SyntaxError`, `URIError`, `AggregateError`, `DOMException`

**Prototype chain:** All error types follow the standard prototype hierarchy. `TypeError.prototype`, `RangeError.prototype`, etc. inherit from `Error.prototype`. This means `new TypeError("msg") instanceof TypeError` is `true` AND `new TypeError("msg") instanceof Error` is `true`. Cross-type checks return `false` (e.g., `new TypeError("msg") instanceof RangeError` is `false`).

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
| `Error.isError(arg)` | Returns `true` if `arg` is an Error instance (has `[[ErrorData]]` internal slot), `false` otherwise |

All error constructors accept an optional second argument `options` with a `cause` property. `AggregateError` takes `(errors, message, options?)` where `errors` is an array of error objects. `DOMException` takes `(message?, name?)` where `name` defaults to `"Error"` — the `code` property is automatically set from the legacy error code mapping (e.g., `"DataCloneError"` → 25).

Each creates an error object with `name`, `message`, and `stack` properties. The `stack` property is a formatted string with the following structure:

```text
ErrorName: message
    at functionName (filePath:line:col)
    at outerFunction (filePath:line:col)
```

### Iterator (`Goccia.Values.IteratorValue.pas`, `Iterator.Concrete.pas`, `Iterator.Lazy.pas`, `Iterator.Concat.pas`, `Iterator.Generic.pas`)

Implements the [ECMAScript Iterator Helpers](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator), Iterator Sequencing (`Iterator.concat`), and the Stage 3 Joint Iteration proposal (`Iterator.zip`).

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

Implements [ECMAScript Symbol](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol). **Not implemented:** `Symbol.unscopables`. Also includes `Symbol.metadata` (TC39 decorator metadata), `Symbol.dispose`, and `Symbol.asyncDispose` (TC39 explicit resource management) beyond the base standard.

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

Implements the [ECMAScript Set](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) including Set methods (`union`, `intersection`, `difference`, `symmetricDifference`, `isSubsetOf`, `isSupersetOf`, `isDisjointFrom`).

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

Implements the [ECMAScript Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) including `Map.prototype.getOrInsert`/`getOrInsertComputed`.

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
| `map.getOrInsert(key, default)` | Return value for key if present; otherwise insert default and return it |
| `map.getOrInsertComputed(key, cb)` | Return value for key if present; otherwise call `cb(key)`, insert result, and return it |

**Static methods:**

| Method | Description |
|--------|-------------|
| `Map.groupBy(iterable, callback)` | Group elements into a Map by callback return value |

Maps are iterable: `[...myMap]` spreads the map into an array of `[key, value]` pairs.

### Promise (`Goccia.Builtins.GlobalPromise.pas`, `Goccia.Values.PromiseValue.pas`)

Implements the [ECMAScript Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) including `Promise.withResolvers()` and `Promise.try()`. See [MDN Promise reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) for the standard API.

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

### Temporal

See [Temporal Built-ins](built-ins-temporal.md) for the complete Temporal API reference (PlainDate, PlainTime, PlainDateTime, Duration, Instant, ZonedDateTime, and more).

### Binary Data (ArrayBuffer, SharedArrayBuffer, TypedArrays)

See [Binary Data Built-ins](built-ins-binary-data.md) for the complete ArrayBuffer, SharedArrayBuffer, and TypedArray API reference.

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

### Test Assertions (`Goccia.Builtins.TestingLibrary.pas`)

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

Only available when `ggBenchmark` is enabled (used by the GocciaBenchmarkRunner). See [benchmarks.md](benchmarks.md) for usage, output formats, and CI integration.

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
| `runBenchmarks()` | Execute all registered benchmarks and return results. Injected automatically by GocciaBenchmarkRunner. |

**Result object** (returned by `runBenchmarks()`):

Each benchmark result includes: `name`, `suite`, `opsPerSec`, `meanMs`, `iterations`, `totalMs`, `variancePercentage`, and optionally `error`.

### Proxy (`Goccia.Builtins.GlobalProxy.pas`, `Goccia.Values.ProxyValue.pas`)

Implements the [ECMAScript Proxy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy). See [MDN Proxy reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy) for the full API.

**GocciaScript differences:** None -- all 13 handler traps are supported with full invariant enforcement. Both string and symbol property keys work. `Proxy.revocable()` is supported.

### Reflect (`Goccia.Builtins.GlobalReflect.pas`)

Implements the [ECMAScript Reflect](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect). See [MDN Reflect reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect) for the full API.

**GocciaScript differences:** None -- all 13 Reflect methods are supported with both string and symbol property keys.

### TextEncoder (`Goccia.Builtins.GlobalTextEncoder.pas`)

Implements the [Encoding Standard TextEncoder](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder). See [MDN TextEncoder reference](https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder) for the full API.

**GocciaScript differences:** None -- full standard compliance (UTF-8 only, with `encode` and `encodeInto`).

### TextDecoder (`Goccia.Builtins.GlobalTextDecoder.pas`)

Implements the [Encoding Standard TextDecoder](https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder). See [MDN TextDecoder reference](https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder) for the full API.

**GocciaScript differences:** Only `"utf-8"` encoding is supported. The `fatal` and `ignoreBOM` options are accepted.

### URL (`Goccia.Builtins.GlobalURL.pas`)

Implements the [WHATWG URL Standard](https://developer.mozilla.org/en-US/docs/Web/API/URL). See [MDN URL reference](https://developer.mozilla.org/en-US/docs/Web/API/URL) for the full API.

**GocciaScript differences:** None -- full standard compliance.

### URLSearchParams (`Goccia.Values.URLSearchParamsValue.pas`)

Implements the [WHATWG URLSearchParams](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams). See [MDN URLSearchParams reference](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams) for the full API.

**GocciaScript differences:** None -- full standard compliance.

### fetch (`Goccia.Builtins.GlobalFetch.pas`)

Implements a subset of the [WHATWG Fetch Standard](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API). Only `GET` and `HEAD` methods are supported; other methods throw `TypeError`.

| Function / Property | Description |
|---------------------|-------------|
| `fetch(url, options?)` | Perform an HTTP request, returns `Promise<Response>` |

The `options` object supports `method` (`"GET"` or `"HEAD"`) and `headers` (plain object or `Headers` instance). Redirects (301/302/303/307/308) are followed automatically up to 20 hops. HTTPS requires OpenSSL libraries at runtime.

**GocciaScript differences:** Only `GET` and `HEAD` methods. No `Request` object, `AbortSignal`, streaming body, or CORS. Requests are synchronous internally; the returned Promise is already settled.

### Headers (`Goccia.Values.HeadersValue.pas`)

Implements the [WHATWG Fetch Headers](https://developer.mozilla.org/en-US/docs/Web/API/Headers) interface.

| Method / Property | Description |
|-------------------|-------------|
| `new Headers()` | Create empty headers |
| `new Headers(object)` | Create from plain object or another `Headers` |
| `headers.get(name)` | Get header value (case-insensitive), or `null` |
| `headers.has(name)` | Check if header exists (case-insensitive) |
| `headers.forEach(callback)` | Iterate entries as `callback(value, name, headers)` |
| `headers.entries()` | Iterator of `[name, value]` pairs |
| `headers.keys()` | Iterator of header names |
| `headers.values()` | Iterator of header values |
| `headers[Symbol.iterator]()` | Same as `entries()` |

**GocciaScript differences:** Read-only on Response headers. No `append`, `set`, or `delete` mutations.

### Response (`Goccia.Values.ResponseValue.pas`)

Implements the [WHATWG Fetch Response](https://developer.mozilla.org/en-US/docs/Web/API/Response) interface.

| Method / Property | Description |
|-------------------|-------------|
| `response.status` | HTTP status code (e.g. `200`) |
| `response.statusText` | HTTP status text (e.g. `"OK"`) |
| `response.ok` | `true` if status is 200–299 |
| `response.url` | Final URL after redirects |
| `response.headers` | `Headers` object |
| `response.type` | Always `"basic"` |
| `response.redirected` | `true` if any redirect was followed |
| `response.bodyUsed` | `true` after a body method is called |
| `response.text()` | Returns `Promise<string>` (UTF-8 decoded body) |
| `response.json()` | Returns `Promise<any>` (parsed JSON body) |
| `response.arrayBuffer()` | Returns `Promise<ArrayBuffer>` (raw bytes) |

**GocciaScript differences:** No `Response.body` (ReadableStream), `blob()`, `formData()`, or `clone()`. Body is fully buffered. Each body method can only be called once.

### DisposableStack / AsyncDisposableStack (`Goccia.Builtins.DisposableStack.pas`)

Implements [Explicit Resource Management](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DisposableStack). See [MDN DisposableStack reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DisposableStack) for the full API.

**GocciaScript differences:** None -- full proposal compliance. Both `DisposableStack` (sync) and `AsyncDisposableStack` (async) are supported. `SuppressedError` is thrown when both the block body and a disposer throw, with `error` (body error) and `suppressed` (dispose error) properties.
