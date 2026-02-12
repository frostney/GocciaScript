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
  ggPromise,          // Promise (placeholder)
  ggJSON,             // JSON.parse, JSON.stringify
  ggSymbol,           // Symbol, Symbol.for, Symbol.keyFor
  ggSet,              // Set constructor and prototype
  ggMap,              // Map constructor and prototype
  ggTestAssertions    // describe, test, expect (testing only)
);
```

The default set used by `ScriptLoader` and `REPL`:

```pascal
DefaultGlobals = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray,
                  ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap];
```

The `TestRunner` adds `ggTestAssertions` to inject the test framework.

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

### Math (`Goccia.Builtins.Math.pas`)

**Constants:**

| Constant | Value |
|----------|-------|
| `Math.PI` | 3.14159265358979... |
| `Math.E` | 2.71828182845904... |
| `Math.LN2` | 0.693147... |
| `Math.LN10` | 2.302585... |
| `Math.SQRT2` | 1.414213... |

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

All methods handle `NaN` and `Infinity` edge cases correctly.

### JSON (`Goccia.Builtins.JSON.pas`)

| Method | Description |
|--------|-------------|
| `JSON.parse(text)` | Parse JSON string to value |
| `JSON.stringify(value)` | Convert value to JSON string |

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
| `Object.defineProperty(obj, prop, desc)` | Define property descriptor |
| `Object.defineProperties(obj, props)` | Define multiple descriptors |
| `Object.getOwnPropertyNames(obj)` | All own property names |
| `Object.getOwnPropertyDescriptor(obj, prop)` | Get property descriptor |
| `Object.getOwnPropertySymbols(obj)` | All own symbol-keyed properties |

### Array (`Goccia.Builtins.GlobalArray.pas`)

| Method | Description |
|--------|-------------|
| `Array.isArray(value)` | Check if value is an array |

Array prototype methods are implemented directly on `TGocciaArrayValue`:

`map`, `filter`, `reduce`, `forEach`, `some`, `every`, `flat`, `flatMap`, `join`, `includes`, `push`, `pop`, `slice`, `toReversed`, `toSorted`, `toSpliced`

### Number (`Goccia.Builtins.GlobalNumber.pas`)

| Method | Description |
|--------|-------------|
| `Number.parseInt(str, radix?)` | Parse integer (supports radix 2-36) |
| `Number.parseFloat(str)` | Parse floating-point |
| `Number.isFinite(value)` | Check if finite number |
| `Number.isNaN(value)` | Check if NaN |
| `Number.isInteger(value)` | Check if integer |

Also aliased as global `parseInt`, `parseFloat`, `isFinite`, `isNaN`.

### String (`Goccia.Builtins.GlobalString.pas`)

String constructor available as `String()`.

String prototype methods are implemented on string values:

`charAt`, `charCodeAt`, `indexOf`, `lastIndexOf`, `includes`, `startsWith`, `endsWith`, `slice`, `substring`, `toLowerCase`, `toUpperCase`, `trim`, `trimStart`, `trimEnd`, `repeat`, `replace`, `split`

### Global Constants and Error Constructors (`Goccia.Builtins.Globals.pas`)

**Constants:** `undefined`, `NaN`, `Infinity`

**Error constructors:** `Error`, `TypeError`, `ReferenceError`, `RangeError`

Each creates a `TGocciaError` with the appropriate `Name` and `Message`.

### Symbol (`Goccia.Builtins.GlobalSymbol.pas`)

Symbols are unique, immutable primitive values used as property keys.

| Method/Property | Description |
|--------|-------------|
| `Symbol(description?)` | Create a new unique symbol |
| `Symbol.for(key)` | Get/create a symbol in the global registry |
| `Symbol.keyFor(symbol)` | Get the key for a global registry symbol |
| `Symbol.iterator` | Well-known symbol constant |

Symbols can be used as computed property keys:

```javascript
const sym = Symbol("myKey");
const obj = { [sym]: "value" };
obj[sym]; // "value"
```

`Object.defineProperty` and `Object.getOwnPropertySymbols` also support symbol keys.

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
| `set.values()` | Get values as an array |

Sets are spreadable: `[...mySet]` produces an array of the set's values.

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
| `map.keys()` | Get keys as an array |
| `map.values()` | Get values as an array |
| `map.entries()` | Get `[key, value]` pairs as an array |

Maps are spreadable: `[...myMap]` produces an array of `[key, value]` pairs.

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
| `.toContain(item)` | Array/string contains |
| `.toBeInstanceOf(class)` | instanceof check |
| `.toHaveLength(n)` | Length check |
| `.toHaveProperty(name)` | Property exists |
| `.toThrow()` | Throws an error |
| `.toBeCloseTo(n, digits?)` | Approximate equality |

All matchers support `.not` negation: `expect(value).not.toBe(wrong)`.
