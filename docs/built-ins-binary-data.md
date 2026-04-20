# Binary Data Built-ins

*ArrayBuffer, SharedArrayBuffer, and TypedArray API reference.*

## Executive Summary

- **ArrayBuffer** — raw binary data buffers with fixed-length and resizable modes, transfer semantics, and detachment
- **SharedArrayBuffer** — fixed-length binary buffer with the same API shape as ArrayBuffer but as a distinct type
- **TypedArrays** — array-like views over buffer data with 12 element types (Int8 through Float64, BigInt64, BigUint64)
- **Uint8Array encoding** — Base64 and hex encoding/decoding
- **Not supported** — `DataView`

## ArrayBuffer (`Goccia.Builtins.GlobalArrayBuffer.pas`, `Goccia.Values.ArrayBufferValue.pas`)

Implements the [ECMAScript ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer). See [MDN ArrayBuffer reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) for the full API.

**Full standard compliance** — includes resizable buffers (`maxByteLength`), `transfer`, and `transferToFixedLength`.

Internally backed by a zero-initialized `TBytes` array. ArrayBuffer instances are cloneable via `structuredClone`.

## SharedArrayBuffer (`Goccia.Values.SharedArrayBufferValue.pas`)

Implements the [ECMAScript SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer). See [MDN SharedArrayBuffer reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) for the full API.

**Full standard compliance.** In GocciaScript, `SharedArrayBuffer` has the same API as `ArrayBuffer` but is a distinct type (not an instance of `ArrayBuffer`). SharedArrayBuffer instances are cloneable via `structuredClone`.

## TypedArrays (`Goccia.Values.TypedArrayValue.pas`)

Implements the [ECMAScript TypedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). See [MDN TypedArray reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for the full API.

**Full standard compliance** for the supported types. All standard constructors, static methods (`from`, `of`), instance properties, and prototype methods are available.

**Supported types:**

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
| `BigInt64Array` | 8 bytes | -2⁶³ to 2⁶³-1 (BigInt) |
| `BigUint64Array` | 8 bytes | 0 to 2⁶⁴-1 (BigInt) |

**Not supported:** `DataView`.

**Value encoding:** Integer types use fixed-width truncation (overflow wraps). `Uint8ClampedArray` clamps to [0, 255] with half-to-even rounding. `Float16Array` rounds to IEEE 754 half precision (max finite ±65504, epsilon 2⁻¹⁰ at 1.0). `Float32Array` rounds to IEEE 754 single precision. `Float64Array` preserves full double precision. NaN is stored as 0 in integer types and as NaN in float types.

### Uint8Array Base64/Hex encoding (`Goccia.Values.Uint8ArrayEncoding.pas`)

[Uint8Array Base64/Hex encoding](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array#base64_and_hex). These methods are available only on `Uint8Array`, not on other TypedArray types.

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

## Related documents

- [Built-in Objects](built-ins.md) — full built-in object reference
- [Language Features](language.md) — language syntax and semantics
