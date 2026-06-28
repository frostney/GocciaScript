# Migrate explicit per-thread cache clears into the thread-cleanup registry

**Date:** 2026-06-28
**Area:** `engine`
**Issue:** [#893](https://github.com/frostney/GocciaScript/issues/893)

[ADR 0078](0078-thread-local-cleanup-registry.md) added `Goccia.ThreadCleanupRegistry` and routed the ~64 member-definition threadvars through it, but to avoid destabilising working teardown in [#891](https://github.com/frostney/GocciaScript/pull/891) it left seven pre-existing per-thread cache/memo clears as **explicit calls** inside `Goccia.Threading.ShutdownThreadRuntime`: `ClearImportMetaCache`, `ShutdownAtomicsWaitersForCurrentThread`, `ClearDisposableStackSlotMap`, `ClearSemverHosts`, `ClearTimeZoneCache`, `ClearRegExpInputMemo`, and `ClearAsciiMemo`. That left two cleanup idioms coexisting on the worker-exit path — explicit calls plus the registry drain — and coupled `Goccia.Threading`'s `uses` clause to seven cache-owning units.

## Decision

Each of the seven units registers its clear with `Goccia.ThreadCleanupRegistry` from its own `initialization` section, the same shape the 64 member-definition threadvars already use, and `ShutdownThreadRuntime` drops the seven explicit calls — keeping only the `RunThreadvarCleanups` drain followed by the ordered object-lifecycle shutdowns (MicrotaskQueue / CallStack / GarbageCollector), which stay explicit because their order matters. The drain still runs **before** those shutdowns, so a cache that touches the collector (`ClearImportMetaCache` unpins) is released while the GC is alive. `Goccia.Threading` no longer `uses` any of the seven units.

Units whose `finalization` existed only to clear a threadvar drop it and rely on the registry's own finalization for main-thread cleanup (`ImportMeta`, `DisposableStack`, `Semver`, `RegExp.VM`). Units whose finalization does more keep that part: `Temporal.TimeZone` retains the Windows ICU-lock teardown, and `Atomics` retains its all-threads shutdown (below).

Three cases needed care:

- **Atomics** is not a managed threadvar. `GAtomicsWaiters` is a shared, lock-guarded global; `ShutdownAtomicsWaitersForCurrentThread` removes only the calling thread's entries (keyed by owner thread id). It is registered for the worker path, but the unit's own `finalization` keeps the all-threads `ShutdownAtomicsWaiters` plus `DoneCriticalSection` for the main thread — preserving the per-thread/all-threads distinction. Because the registry finalizes *after* this unit (it is an earlier-initialised dependency), the registry's main-thread drain would otherwise call the per-thread proc *after* the lock is destroyed; a pre-lock `if not Assigned(GAtomicsWaiters)` guard makes that a safe no-op. The registry's callback contract is broadened to cover "a thread's own entries in a shared lock-guarded structure," not only managed threadvars.

- **TextSemantics** is generic shared infrastructure (`source/shared/`) with no engine dependency, used by the JSON, numeric-text and CLI-config tools as well as the engine. To keep it engine-free, its is-ASCII memo (#806) clear is registered from the engine's `Goccia.RegExp.VM` rather than self-registered — every engine binary links the regex VM, so the worker path is covered. Its **own main-thread `finalization` is retained**, because binaries that link `TextSemantics` but not `RegExp.VM` (the shared tools and `TextSemantics.Test`) populate the memo and would otherwise never release it.

- **ImportMeta** keeps `ClearImportMetaCache` exported: besides the registry, the engine calls it directly during its own teardown.

## Consequences

- Worker-thread cleanup is one mechanism (the registry drain); `Goccia.Threading` is decoupled from the cache-owning units. Adding a new per-thread cache no longer means editing `ShutdownThreadRuntime`.
- Main-thread cleanup is preserved on whichever path is correct per unit: the registry finalization for the self-registering engine units, and the unit's own finalization for `Atomics` (all-threads), the Windows ICU lock, and `TextSemantics` (binary-independent).
- A new gate in `Goccia.ThreadCleanupLeak.Test` pins each of the seven registrations via `IsThreadvarCleanupRegistered`, so a dropped `RegisterThreadvarCleanup` fails loudly instead of silently leaking. `Goccia.Threading.Test` still proves the drain fires once per worker exit. The full JavaScript suite passes in both modes and all Pascal unit tests pass.
- No behaviour or conformance change: this is the cleanliness/embeddability follow-up ADR 0078 deferred, not a runtime change.
