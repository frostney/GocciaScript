# Explicit bounded FFI call descriptors

**Date:** 2026-07-24
**Area:** `ffi`, `runtime`
**Issues:** [#1029](https://github.com/frostney/GocciaScript/issues/1029), [#1030](https://github.com/frostney/GocciaScript/issues/1030), [#1031](https://github.com/frostney/GocciaScript/issues/1031), [#1032](https://github.com/frostney/GocciaScript/issues/1032), [#1033](https://github.com/frostney/GocciaScript/issues/1033)
**Related:** [ADR 0095](0095-custom-bidirectional-ffi-abi-engine.md)

GocciaScript extends its custom compiled-signature ABI engine rather than
adding a second foreign-call subsystem. Bound functions and callbacks may have
up to 64 arguments, and every top-level argument is independently classified,
including mixed `f32`, integer, pointer, and aggregate signatures. The bound
call and reverse-callback paths continue to consume the same immutable ABI
placements.

Variadic native functions use an explicit two-part contract. The signature
declares its ordinary fixed prefix with `variadic: true`. Each invocation then
supplies exactly one final `FFI.varargs(types, values)` marker. The marker's
parallel arrays must have equal lengths and the combined call must remain within
the 64-argument bound. Before native entry, the planner applies the C default
argument promotions and compiles the complete call signature. Target-specific
placement handles Windows x64 floating-point register duplication and Darwin
ARM64 variadic stack rules. Variadic callbacks are rejected.

Nullability is also explicit and compositional. `FFI.nullable("utf8string")`
accepts strict UTF-8/NUL-checked text or JavaScript `null`, which maps to native
`NULL`. It is valid only in bound native-function argument positions. Plain
`utf8string` keeps its existing coercion behavior. Other wrapped types,
aggregate storage, returns, and callback positions are rejected until their
ownership and lifetime contracts are designed.

Aggregate definitions preserve exact native field names, including `buffer` and
`byteOffset`. Declared fields take precedence over the legacy direct metadata
properties. `FFI.metadata(value)` is the universal, non-colliding access path
for backing `buffer`, `byteOffset`, and `size`, plus `length` for arrays.
Non-colliding aggregate values retain their direct metadata properties.

Alternatives considered:

- **Adopt libffi for variadic and high-arity calls.** Rejected because the
  existing custom planner already models all supported targets and is shared
  with reverse callbacks. A second ABI engine would make behavior drift.
- **Infer variadic tails from extra JavaScript arguments.** Rejected because
  JavaScript values do not carry the native type information required for ABI
  classification and C default promotions.
- **Add more fixed-arity scalar dispatcher cases.** Rejected because the
  compiled signature planner already supports register, stack, aggregate, and
  hidden-return interactions without combinatorial dispatch tables.
- **Reserve aggregate metadata property names.** Rejected because native field
  names must be represented exactly. A universal helper avoids collisions while
  retaining compatible direct access where no collision exists.
- **Treat every pointer-like descriptor as nullable.** Rejected because string,
  pointer, aggregate, and callback lifetimes have different ownership
  contracts. The initial wrapper is deliberately limited to UTF-8 arguments.

Consequences:

- Each supported target must validate high-arity and variadic placement in its
  CI lane.
- Variadic calls are explicit at both bind and invocation sites and cannot
  silently accept an untyped tail.
- Future nullable types or positions require a separate lifetime and ownership
  decision rather than inheriting `utf8string` behavior accidentally.
- Aggregate consumers that declare metadata-colliding fields must use
  `FFI.metadata(value)` for backing-store inspection.
