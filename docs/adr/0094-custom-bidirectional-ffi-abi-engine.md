# Custom bidirectional FFI ABI engine

**Date:** 2026-07-13
**Area:** `ffi`, `runtime`
**Issue:** [#827](https://github.com/frostney/GocciaScript/issues/827)
**Related:** [#214](https://github.com/frostney/GocciaScript/issues/214), [#215](https://github.com/frostney/GocciaScript/issues/215), [#216](https://github.com/frostney/GocciaScript/issues/216), [#224](https://github.com/frostney/GocciaScript/issues/224), [#225](https://github.com/frostney/GocciaScript/issues/225)

FFI v2 extends GocciaScript's existing custom native-call machinery into one
bidirectional ABI engine. An immutable compiled signature plan classifies every
argument and return value for the current platform ABI. Outgoing calls use that
plan to place scalar and aggregate bytes in registers, stack slots, indirect
copies, and hidden-result storage. Native callback entries use the same plan in
reverse to reconstruct JavaScript arguments and marshal the callback result.
The implementation does not use libffi.

FFI type descriptors are recursive and compositional. Structures, unions, and
fixed-length arrays use the platform C ABI's natural size, alignment, padding,
argument, and return rules. They may nest and may appear by value in function or
callback signatures. Aggregate values use `ArrayBuffer` storage so nested views
share the same bytes. The public API deliberately excludes packed layouts,
bitfields, custom alignment, and non-native calling conventions.

Native callback entry points come from a bounded table of statically compiled
stubs for each supported target. Passing a JavaScript callable to a
callback-typed argument creates a call-scoped callback that is closed when the
native call returns. Calling `CallbackType.create(callable)` creates a
persistent callback handle that remains pinned until its idempotent `close()`
method is called. Callbacks may re-enter GocciaScript only on the runtime thread
that created them. A foreign-thread invocation returns the ABI's zero value and
latches a `TypeError` for the owning call. A JavaScript exception follows the
same native-boundary rule: the first exception is retained, later callbacks in
that native call return zero without invoking JavaScript, and the original
exception is rethrown after native code returns. No Pascal exception may unwind
through a native callback frame.

Dynamic-library lifetime uses a shared guard retained by the library object,
bound functions, and symbol pointers. `library.close()` is a logical close: it
immediately invalidates the library and every derived callable or pointer, while
physical unloading waits until the last dependent releases its guard. This
prevents both calls through unloaded code and premature unloading when the
library object is collected before its dependents.

Alternatives considered:

- **Adopt libffi.** Rejected because GocciaScript already owns working custom
  calling machinery across its supported targets, and FFI v2 needs one coherent
  plan shared by outgoing calls and reverse callback entry rather than a second
  foreign-call subsystem.
- **Add aggregate and callback cases to the scalar dispatcher independently.**
  Rejected because ABI classification, hidden returns, indirect arguments, and
  register exhaustion interact. Per-feature branches would duplicate those
  rules and make outgoing and callback behavior drift.
- **Unload immediately on `close()`.** Rejected because bound functions and raw
  symbol pointers would retain dangling addresses. Logical invalidation plus
  deferred unload preserves explicit close semantics without use-after-unload.
- **Allow callbacks from arbitrary native threads.** Rejected because the
  evaluator, realm, garbage collector, and call context belong to one runtime
  thread. Returning zero and surfacing a deferred error gives a deterministic
  boundary without pretending the runtime is cross-thread re-entrant.

Consequences:

- Every supported FFI target must provide matching outgoing and callback entry
  assembly and must be exercised by the platform CI matrix.
- Extending the type system or adding a calling convention now happens in the
  compiled-plan layer rather than in JavaScript-value marshalling.
- Persistent callbacks and native code follow an explicit ownership contract:
  native code must stop retaining a callback before its handle is closed.
- FFI remains an unsafe runtime opt-in. These guards remove known lifetime and
  thread-boundary hazards but do not make arbitrary native code memory-safe.
