# Engine-owned capability audit seam

**Date:** 2026-07-16
**Area:** `engine`, `runtime`, `sandbox`, `cli`
**Issue:** [#882](https://github.com/frostney/GocciaScript/issues/882)

Capability observability uses one typed sink owned by `TGocciaEngine`.
The engine constructs the versioned event, captures the active interpreter or
bytecode call site, and delivers it synchronously. Runtime extensions and core
built-ins receive only a narrow typed emitter callback, so fetch, FFI,
Function construction, ShadowRealm construction, and sandbox filesystem paths
do not depend on CLI logging or serialization.

ShadowRealm child engines inherit the parent sink. Disabled `FFI` and
`ShadowRealm` remain absent globals: auditing must not change JavaScript
feature detection by installing throwing stubs. Their audit events therefore
describe permitted use after installation; real deny gates such as the fetch
allowlist and disabled Function construction report both decisions.

The sandbox virtual filesystem remains engine-independent. It reports root
clamping through a neutral callback, and the sandbox runtime translates that
signal into one deny event per path normalization request. The existing
clamped in-jail operation still proceeds, while ordinary filesystem activity
does not create audit noise.

CLI hosts expose `--audit-log=<path>` as a thread-safe UTF-8 JSONL sink.
Opening and delivery are fail-closed: any file or host-sink exception aborts
execution rather than silently producing an incomplete security record.
Fetch events are emitted on the runtime thread before asynchronous HTTP work,
so audit sinks are never called from fetch workers.

This keeps capability policy at its existing boundaries, centralizes event
shape and child-engine inheritance, and avoids a runtime service lookup or
independent callback contract for every capability.
