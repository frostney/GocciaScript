# FFI Built-ins

*For script authors calling native libraries through GocciaScript's unsafe FFI runtime extension.*

## Executive Summary

- **Explicit opt-in** — FFI is available only through `TGocciaFFIRuntimeExtension`; CLI tools install it for `--unsafe-ffi` or `"unsafe-ffi": true`
- **Compositional native types** — `FFI.struct`, `FFI.union`, and `FFI.array` describe naturally aligned native layouts that can nest and pass by value
- **Bidirectional calls** — `library.bind` calls native functions, while `FFI.callback` exposes JavaScript callables to native code through GocciaScript's custom ABI machinery
- **Runtime-thread callbacks** — callbacks may enter JavaScript only on their owning runtime thread, and failures are deferred until the enclosing native call returns
- **Guarded lifetimes** — closing a library invalidates its dependents immediately and delays physical unloading until those dependents are released
- **Unsafe boundary** — guards cover documented engine-side hazards, but arbitrary native code remains outside JavaScript memory safety

## Runtime Opt-in

The Foreign Function Interface calls native shared libraries. It is available only when `TGocciaFFIRuntimeExtension` is installed. CLI tools install that extension for `--unsafe-ffi` or `"unsafe-ffi": true` in configuration.

## FFI Global Object

| Method/Property | Description |
|--------|-------------|
| `FFI.open(path)` | Open a dynamic library, returning an `FFILibrary` |
| `FFI.struct(fields)` | Declare a native-layout structure type. Field declaration order determines layout. |
| `FFI.union(fields)` | Declare a native-layout union type whose fields share storage. |
| `FFI.array(elementType, length)` | Declare a fixed-length inline array type. |
| `FFI.callback(signature)` | Declare a native callback type. `signature` uses the same `{ args, returns }` shape as `library.bind`. |
| `FFI.nullable("utf8string")` | Declare an explicit nullable UTF-8 string type for a bound native-function argument. |
| `FFI.varargs(types, values)` | Create the one explicit typed tail required by a variadic bound function call. |
| `FFI.metadata(value)` | Return aggregate backing metadata without conflicting with exact native field names. |
| `FFI.nullptr` | Singleton null pointer value |
| `FFI.suffix` | Platform library suffix (`.dylib`, `.so`, `.dll`) |

## FFILibrary Methods

| Method/Property | Description |
|--------|-------------|
| `library.bind(funcName, signature)` | Bind a native function. Each entry in `{ args, returns }` is a scalar type name or FFI type descriptor. Returns a callable function. |
| `library.symbol(name)` | Get a raw pointer to a named symbol |
| `library.close()` | Logically close the library and invalidate its bound functions and symbol pointers. Native unloading waits until all dependents are released. |
| `library.path` | Full path to the loaded library |
| `library.closed` | Whether the library has been closed |

## FFIPointer Properties

| Property | Description |
|----------|-------------|
| `ptr.address` | Numeric address |
| `ptr.isNull` | Whether the pointer is null |

## Type Descriptors and Aggregate Values

Supported scalar types are `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `pointer`, `utf8string`, `bool`, and `void` (return only). `i64`/`u64` are not available on i386. Top-level `f32` arguments may be mixed with integer, pointer, and aggregate arguments; the ABI planner assigns every argument independently.

`FFI.struct`, `FFI.union`, and `FFI.array` return compositional type descriptors. Descriptors expose `kind`, `size`, and `alignment`; array descriptors also expose `length`. Calling `descriptor.create(initializer)` returns an aggregate value backed by an `ArrayBuffer`. Structure and union fields use property access, arrays use numeric indices, and nested aggregate reads return views sharing the root buffer. Aggregate values expose `buffer`, `byteOffset`, and `size` (plus `length` for arrays) when those names do not collide with declared fields. Exact native field names take precedence. `FFI.metadata(value)` always returns the backing `buffer`, `byteOffset`, and `size`, plus `length` for arrays. Detaching that buffer invalidates the root value and all nested views.

Layouts follow the current platform's native C ABI, including natural alignment, padding, register classification, indirect arguments, and hidden returns. Packed layouts, bitfields, custom alignment, non-native calling conventions, and bare C array-parameter decay are not modeled. Use `FFI.array` for inline array storage, such as an array field or the ABI shape of a wrapper aggregate.

```javascript
const Bytes4 = FFI.array("u8", 4);
const Word = FFI.union({ asU32: "u32", asBytes: Bytes4 });
const Point = FFI.struct({ x: "f64", y: "f64" });
const Packet = FFI.struct({ header: Bytes4, payload: Word, point: Point });

const transform = library.bind("transform_packet", {
  args: [Packet],
  returns: Packet,
});

const packet = Packet.create({
  header: Bytes4.create([1, 2, 3, 4]),
  payload: Word.create({ asU32: 5 }),
  point: Point.create({ x: 6, y: 7 }),
});

const result = transform(packet);
result.point.x;
```

Pointer arguments accept `FFIPointer`, `ArrayBuffer`, `SharedArrayBuffer`, `TypedArray`, FFI aggregate values, persistent callback handles, or `null`.

## Nullable UTF-8 Arguments

Plain `utf8string` retains its existing JavaScript string-coercion behavior. Use `FFI.nullable("utf8string")` when JavaScript `null` must map to a native null pointer:

```javascript
const stringLength = library.bind("string_length", {
  args: [FFI.nullable("utf8string")],
  returns: "i32",
});

stringLength("hello"); // 5
stringLength(null);    // native NULL
```

Non-null values must be strings containing well-formed UTF-16 without embedded NUL characters. GocciaScript encodes them into temporary NUL-terminated UTF-8 buffers that are valid only for the duration of the native call; native code must not retain those pointers after the call returns. Nullable descriptors currently support only `utf8string`, only in bound native-function argument positions. Aggregate storage, return types, and callback signatures reject them so the API does not imply unsupported ownership or lifetime semantics.

## Variadic Native Calls

Set `variadic: true` on a bound native-function signature and pass exactly one `FFI.varargs(types, values)` marker after the ordinary fixed arguments. The type and value arrays must have equal lengths. GocciaScript validates and plans the complete call before entering native code.

```javascript
const sum = library.bind("sum_values", {
  args: ["i32"],
  variadic: true,
  returns: "i32",
});

sum(3, FFI.varargs(["i8", "i16", "i32"], [10, 20, 12]));
```

The variadic planner applies the C default argument promotions: `f32` becomes `f64`, while `bool`, `i8`, `i16`, `u8`, and `u16` become `i32`. Other supported argument descriptors retain their ordinary ABI classification. Variadic callbacks are not supported.

## Callbacks

`FFI.callback({ args, returns })` declares the ABI signature native code uses to call JavaScript. Passing a JavaScript callable directly to a callback-typed argument creates a call-scoped callback that is pinned for the native call and closed when that call returns. Call-scoped callbacks are non-escapable: native code must not retain or invoke their pointers after the native call returns. Use `CallbackType.create(callable)` when native code must retain the function pointer across calls; the returned persistent handle exposes `address`, `closed`, and an idempotent `close()` method. Native code must stop retaining the pointer before the handle is closed.

```javascript
const Compare = FFI.callback({
  args: ["pointer", "pointer"],
  returns: "i32",
});

const sort = library.bind("sort_values", {
  args: ["pointer", "i32", Compare],
  returns: "void",
});

sort(values, values.length, (left, right) => compareValues(left, right));

const persistent = Compare.create(compareValues);
registerComparator(persistent);
// Native code unregisters the pointer before this call.
persistent.close();
```

Callbacks may enter JavaScript only on the runtime thread that created them. A foreign-thread invocation returns the callback ABI's zero value and raises `TypeError` after the enclosing native call returns. If JavaScript throws, native code receives the zero value; later callbacks in that native call are suppressed, and the original exception is rethrown after native code returns. No Pascal or JavaScript exception unwinds through a native callback frame. Callback `utf8string` returns are rejected because their storage lifetime cannot be guaranteed.

## Limits

- Function and callback signatures support at most 64 arguments. For variadic calls, this limit applies to the combined fixed prefix and explicit tail.
- At most 64 persistent or call-scoped callbacks may be active in the process at once. Closing a persistent handle or returning from a call-scoped callback releases its slot.
- Structures and unions support at most 256 fields, descriptor nesting is limited to 32 levels, and one aggregate may occupy at most 65,536 bytes.
- FFI remains an unsafe runtime opt-in. Lifetime and thread guards prevent the documented engine-side hazards but cannot make arbitrary native code memory-safe.

## Related Documentation

- [Built-in Objects](built-ins.md) — index of core and runtime built-ins
- [ADR 0095](adr/0095-custom-bidirectional-ffi-abi-engine.md) — custom bidirectional ABI architecture and trade-offs
- [ADR 0102](adr/0102-explicit-bounded-ffi-call-descriptors.md) — explicit bounded high-arity, variadic, aggregate-metadata, and nullable-call contracts
