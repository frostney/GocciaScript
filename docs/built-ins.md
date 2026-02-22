# Built-in Objects

GocciaScript provides a set of built-in global objects that mirror JavaScript's standard library. Each built-in is implemented as a Pascal unit and registered by the engine based on configuration flags.

## Registration System

Built-ins are controlled by the `TGocciaGlobalBuiltins` flag set:

```pascal
TGocciaGlobalBuiltin = (
  ggConsole,          // console.log, etc.
  ggMath,             // Math.PI, Math.floor, etc.
  ggGlobalObject,     // Object.keys, Object.assign, etc.
  ggGlobalArray,      // Array.isArray
  ggGlobalNumber,     // Number.parseInt, Number.isNaN, etc.
  ggPromise,          // Promise constructor, prototype, static methods, microtask queue
  ggJSON,             // JSON.parse, JSON.stringify
  ggSymbol,           // Symbol, Symbol.for, Symbol.keyFor
  ggSet,              // Set constructor and prototype
  ggMap,              // Map constructor and prototype
  ggTestAssertions,   // describe, test, expect (testing only)
  ggBenchmark,        // suite, bench, runBenchmarks (benchmarking only)
  ggTemporal,         // Temporal namespace (dates, times, durations, instants)
  ggJSX              // JSX transformer support
);
```

The default set used by `ScriptLoader` and `REPL`:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                  ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggTemporal, ggJSX];
```

The `TestRunner` adds `ggTestAssertions` to inject the test framework.
The `BenchmarkRunner` adds `ggBenchmark` to inject the benchmark framework.

## Adding a New Built-in

To add a new built-in object:

1. **Create a unit** — `units/Goccia.Builtins.YourBuiltin.pas`
2. **Extend `TGocciaBuiltin`** — Implement the `Register` method.
3. **Register methods** — Create a `TGocciaObjectValue` and attach `TGocciaNativeFunction` callbacks.
4. **Add a flag** — Add to `TGocciaGlobalBuiltin` enum.
5. **Register in Engine** — Add a `RegisterYourBuiltin` method to `TGocciaEngine`.
6. **Write tests** — Add test files under `tests/built-ins/YourBuiltin/`.

### Example: Implementing a Built-in Method

```pascal
function MathFloor(Args: TGocciaValueArray; ThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
begin
  Num := Args[0].ToNumberLiteral;
  if Num.IsSpecialValue then
    Exit(Num);  // NaN, Infinity pass through
  Result := TGocciaNumberLiteralValue.Create(Floor(Num.Value));
end;
```

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
| `Math.fround(x)` | Nearest 32-bit float |
| `Math.hypot(x, y)` | Square root of sum of squares |
| `Math.imul(a, b)` | 32-bit integer multiplication |
| `Math.log1p(x)` | ln(1 + x) (precise for small x) |
| `Math.log2(x)` | Base-2 logarithm |
| `Math.clz32(x)` | Count leading zeros (32-bit) |

All methods handle `NaN` and `Infinity` edge cases correctly.

### JSON (`Goccia.Builtins.JSON.pas`)

| Method | Description |
|--------|-------------|
| `JSON.parse(text, reviver?)` | Parse JSON string to value, with optional reviver function |
| `JSON.stringify(value, replacer?, space?)` | Convert value to JSON string, with optional replacer (function or array) and indentation |

The JSON parser is a recursive descent implementation. Special handling:
- `NaN` → `null` in stringify
- `undefined` → omitted in objects, `null` in arrays
- Functions → omitted

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

### Array (`Goccia.Builtins.GlobalArray.pas`)

**Static methods:**

| Method | Description |
|--------|-------------|
| `Array.isArray(value)` | Check if value is an array |
| `Array.from(iterable, mapFn?)` | Create array from iterable (array or string) with optional map |
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

String prototype methods are implemented on string values:

`charAt`, `charCodeAt`, `codePointAt`, `indexOf`, `lastIndexOf`, `includes`, `startsWith`, `endsWith`, `slice`, `substring`, `toLowerCase`, `toUpperCase`, `trim`, `trimStart`, `trimEnd`, `repeat`, `replace`, `replaceAll`, `split`, `padStart`, `padEnd`, `concat`, `at`, `localeCompare`, `normalize`, `isWellFormed`, `toWellFormed`, `[Symbol.iterator]`

`[Symbol.iterator]()` returns an iterator that yields individual characters.

### Global Constants, Functions, and Error Constructors (`Goccia.Builtins.Globals.pas`)

These are always registered (not flag-gated).

**Constants:** `undefined`, `NaN`, `Infinity`, `globalThis`

`globalThis` is a `const` binding that holds a plain object populated with all global scope bindings at the time of registration. It includes a self-referential `globalThis` property (`globalThis.globalThis === globalThis`).

**`GocciaScript` object:**

A `const` global providing engine metadata:

| Property | Type | Description |
|----------|------|-------------|
| `version` | `string` | Semver version from the latest git tag (e.g., `"0.2.0"`), or tag + `-dev` suffix if there are commits after the tag (e.g., `"0.2.0-dev"`) |
| `commit` | `string` | Short git commit hash (e.g., `"a1b2c3d"`) |
| `builtIns` | `string[]` | Names of the enabled `TGocciaGlobalBuiltin` flags (e.g., `["Console", "Math", "GlobalObject", ...]`), derived via RTTI at runtime |

**Global functions:**

| Function | Description |
|----------|-------------|
| `queueMicrotask(callback)` | Enqueue a callback to run as a microtask. Throws `TypeError` if the argument is not callable. |
| `structuredClone(value)` | Deep-clone a value using the structured clone algorithm. Handles objects, arrays, `Map`, `Set`, and circular references. Throws `DOMException` with name `"DataCloneError"` (code 25) for non-cloneable types (functions, symbols). |

`queueMicrotask` shares the same microtask queue used by Promise reactions. Callbacks run after the current synchronous code completes but before the engine returns control. If a callback throws, the error is silently discarded and remaining microtasks still execute.

`structuredClone` creates a deep copy following the HTML spec's structured clone algorithm. Primitives are returned as-is. Objects, arrays, Maps, and Sets are recursively cloned. Circular references and shared references within the object graph are preserved (the same cloned object is reused). Non-serializable values (functions, symbols) throw a `DOMException` with `name: "DataCloneError"` and `code: 25`, matching browser and Node.js behavior. Accessor properties (getters/setters) are read via the getter and the resulting value is cloned as a data property on the clone.

**Error constructors:** `Error`, `TypeError`, `ReferenceError`, `RangeError`, `SyntaxError`, `URIError`, `AggregateError`, `DOMException`

All error constructors accept an optional second argument `options` with a `cause` property. `AggregateError` takes `(errors, message, options?)` where `errors` is an array of error objects. `DOMException` takes `(message?, name?)` where `name` defaults to `"Error"` — the `code` property is automatically set from the legacy error code mapping (e.g., `"DataCloneError"` → 25).

Each creates an error object with `name`, `message`, and `stack` properties. The `stack` property is a formatted string with the following structure:

```
ErrorName: message
    at functionName (filePath:line:col)
    at outerFunction (filePath:line:col)
```

Runtime errors thrown by the engine (e.g., `TypeError` from accessing a property on `undefined`) also receive a `stack` property.

### Iterator (`Goccia.Values.IteratorValue.pas`, `Iterator.Concrete.pas`, `Iterator.Lazy.pas`, `Iterator.Generic.pas`)

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

**Static method:**

| Method | Description |
|--------|-------------|
| `Iterator.from(value)` | Wrap an iterable, iterator, or `{next()}` object as a proper Iterator with helper methods |
| `Iterator.prototype` | The shared iterator prototype (accessible for inspection) |

### Symbol (`Goccia.Builtins.GlobalSymbol.pas`)

Symbols are unique, immutable primitive values used as property keys.

| Method/Property | Description |
|--------|-------------|
| `Symbol(description?)` | Create a new unique symbol |
| `Symbol.for(key)` | Get/create a symbol in the global registry |
| `Symbol.keyFor(symbol)` | Get the key for a global registry symbol (throws `TypeError` for non-symbol arguments) |
| `Symbol.iterator` | Well-known symbol for iteration protocol |
| `Symbol.species` | Well-known symbol for constructor species (behavioral: Array/Map/Set constructors have default `[Symbol.species]` getter returning `this`; Array prototype methods use `ArraySpeciesCreate` for subclass-aware result construction) |
| `Symbol.hasInstance` | Well-known symbol for instanceof behavior |
| `Symbol.toPrimitive` | Well-known symbol for type conversion |
| `Symbol.toStringTag` | Well-known symbol for Object.prototype.toString |
| `Symbol.isConcatSpreadable` | Well-known symbol for Array.prototype.concat |
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

### Temporal (`Goccia.Builtins.Temporal.pas`)

An implementation of the ECMAScript Temporal API (Stage 3 proposal) providing modern date/time handling. ISO 8601 calendar only. All Temporal types are immutable — operations return new instances.

**Namespace structure:**

```javascript
Temporal.Now          // Current time utilities
Temporal.Duration     // Time duration representation
Temporal.Instant      // Absolute point in time (epoch-based)
Temporal.PlainDate    // Calendar date (no time/timezone)
Temporal.PlainTime    // Wall-clock time (no date/timezone)
Temporal.PlainDateTime // Date + time (no timezone)
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
| `toString()` / `toJSON()` | ISO 8601 duration string (e.g., `"P1Y2M3DT4H5M6S"`) |
| `valueOf()` | Throws TypeError (prevents implicit coercion) |

#### Temporal.PlainDate

Represents a calendar date without time or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainDate(year, month, day)` | Create from components |
| `Temporal.PlainDate.from(item)` | Create from string (`"2024-03-15"`), PlainDate, or object |
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
| `round(unit)` | Round to nearest unit |
| `equals(other)` | Equality check |
| `toString()` / `toJSON()` | ISO time string (e.g., `"13:45:30"`) |
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
| `round(unit)` | Round to nearest unit |
| `equals(other)` | Equality check |
| `toPlainDate()` / `toPlainTime()` | Extract date or time component |
| `toString()` / `toJSON()` | ISO string (e.g., `"2024-03-15T13:45:30"`) |
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
| `round(unit)` | Round to nearest unit |
| `equals(other)` | Equality check |
| `toString()` / `toJSON()` | ISO string with UTC (e.g., `"2024-03-15T13:45:30Z"`) |
| `valueOf()` | Throws TypeError |

#### Temporal.Now

Provides current time in various representations.

| Method | Description |
|--------|-------------|
| `Temporal.Now.instant()` | Current time as Instant |
| `Temporal.Now.plainDateISO()` | Current date as PlainDate |
| `Temporal.Now.plainTimeISO()` | Current time as PlainTime |
| `Temporal.Now.plainDateTimeISO()` | Current date-time as PlainDateTime |

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

**Functions:** `describe`, `test`, `it` (alias for `test`), `test.skip`, `beforeEach`, `afterEach`

**Matchers:**

| Matcher | Description |
|---------|-------------|
| `.toBe(expected)` | Strict equality (`===`) |
| `.toEqual(expected)` | Deep equality |
| `.toBeNull()` | Is null |
| `.toBeNaN()` | Is NaN |
| `.toBeUndefined()` | Is undefined |
| `.toBeTruthy()` | Truthy value |
| `.toBeFalsy()` | Falsy value |
| `.toBeGreaterThan(n)` | Greater than |
| `.toBeLessThan(n)` | Less than |
| `.toContain(item)` | Array/Set element or string substring |
| `.toBeInstanceOf(class)` | instanceof check |
| `.toHaveLength(n)` | Length check |
| `.toHaveProperty(name)` | Property exists on an object. Non-object values always fail; negated (`not.toHaveProperty`) on non-objects passes. |
| `.toThrow(ErrorType?)` | Throws an error (optionally checks error constructor) |
| `.toBeCloseTo(n, digits?)` | Approximate equality |

All matchers support `.not` negation: `expect(value).not.toBe(wrong)`.

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
