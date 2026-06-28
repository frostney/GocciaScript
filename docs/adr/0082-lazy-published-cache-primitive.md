# Unify embedded-data caches onto a lock-free publication primitive

**Date:** 2026-06-28
**Area:** `engine`
**Issue:** [#894](https://github.com/frostney/GocciaScript/issues/894)

The engine lazily loads several immutable embedded-data tables — Unicode property ranges, RegExp case-fold and non-Unicode-uppercase pairs, the IANA time-zone and CLDR resource blobs, and the available-locale list — and then reads them concurrently from the regex/lexer worker-thread pool. [#813](https://github.com/frostney/GocciaScript/issues/813) converted the RegExp case-fold/uppercase accessor from "lock on every read" to a double-checked lock-free publication: the `Loaded` flag is published last behind a `WriteBarrier` and read behind a matching `ReadBarrier`, so a reader that observes `Loaded = True` also observes the fully-written table on weakly-ordered targets (AArch64). That was the first memory-barrier use in the engine and was kept deliberately local.

The sibling caches shared the same lazy-init shape **without** the barrier discipline, in three different and individually-incorrect or costly ways:

- `Goccia.Temporal.TimeZoneData` and `Goccia.Intl.CLDRData` read and wrote their `Cached…Resource` / `Cached…ResourceLoaded` globals with **no synchronization at all** — both a cold-load data race and a publication gap on weakly-ordered CPUs.
- `Goccia.RegExp.UnicodeData`'s own `TryReadEmbeddedResource` (the UCD blob) and `Goccia.Identifier` / `IntlLocaleResolver` (identifier ID_Start/ID_Continue ranges and the available-locale list) were **correct but always-locked**: they entered a `TRTLCriticalSection` and copied the cached dynamic array out on *every* call, including the lexer's per-non-ASCII-code-point identifier hot path.

## Decision

Introduce one generic record, `TLazyPublishedCache<T>` in `source/shared/LazyPublishedCache.pas`, that owns the lazy one-shot load plus barrier-correct lock-free publication for an immutable value of any type `T`. It bundles `Data`, the `Loaded`/`Available` flags and the `Lock` into a single record, and exposes `Init`/`Done` (lock lifecycle) and `Ensure(const AKey; const ALoader): Boolean`. `Ensure` runs the cold load once under the lock, publishes `Loaded` last behind a `WriteBarrier`, and serves the warm path lock-free behind a `ReadBarrier`; load failure is memoized (`Loaded = True, Available = False`) so an absent or corrupt resource is not re-attempted. Each consumer supplies a small unit-level loader `function(const AKey; out AData: T): Boolean` and reads `Cache.Data` in place via a `const` argument, which makes no managed copy.

All eight embedded-data caches across five units now consume it:

| Unit | Caches | Was |
|------|--------|-----|
| `Goccia.Temporal.TimeZoneData` | TZ resource blob | unsynchronized |
| `Goccia.Intl.CLDRData` | CLDR resource blob | unsynchronized |
| `Goccia.RegExp.UnicodeData` | UCD blob; case-fold pairs; non-Unicode-uppercase pairs | lock-only + hand-rolled barrier DCL (#813) |
| `Goccia.Identifier` | ID_Start ranges; ID_Continue ranges | always-locked, copies out |
| `IntlLocaleResolver` | available-locale list | always-locked, copies out |

The barrier discipline now lives in exactly one place instead of being re-asserted (or omitted) per call site, a mismatched `(data, flag, lock)` pairing is a compile-time error, and the warm-path zero-copy read that #813 gave the case-fold tables now also covers the lexer's identifier hot path.

## Rejected alternatives

- **Per-unit duplication of the idiom (status quo extended).** Apply the #813 barrier DCL in-place to each cache. Correct, but re-asserts the ordering at every site, keeps the loose `(array, flag, lock)` globals, and was exactly what this issue set out to remove.
- **Two-phase record API with no callback** (`TryWarm` / `BeginColdLoad` / `Publish`). Avoids the loader function pointer, but only *partly* centralizes the discipline: every one of the eight sites must still order the phases correctly, so the barrier rope stays in the callers.
- **A concrete `TBytes` resource-buffer helper plus separate handling for the derived tables.** Removes the most duplicate concrete code (the three near-identical resource readers) but introduces two abstractions and still leaves the pair/range/locale caches without a shared publication primitive — not the single primitive the issue targeted.

## Consequences

- Behavior-preserving: the full JavaScript suite passes in both interpreter and bytecode modes, and the affected RegExp/Intl/Temporal areas pass 100%. The publication ordering is the only semantic change.
- TimeZone and CLDR resource loads now take a one-shot cold-load lock (closing the data race) and memoize load failure (previously they retried the resource lookup on every miss).
- FPC lowers the barriers to `dmb ishld` / `dmb ishst` on AArch64 and to compiler barriers on x86 (TSO), so publication is correct on both without a per-read lock.
- A weak-memory publication race is non-deterministic and not reproducible by a JavaScript test, so the primitive's functional contract (load-once, memoize-failure, data-visible-iff-available, payload-agnostic) is locked in by the Pascal gate `LazyPublishedCache.Test`, matching the [thread-local cleanup](0078-thread-local-cleanup-registry.md) precedent of Pascal gates for concurrency infrastructure.
- Audit (per the issue): `Goccia.Temporal.TimeZone`'s `Cached…` tables are `threadvar` (per-thread, no cross-thread publication) and the ICU `EnsureLoaded` / `WindowsICULoadAttempted` paths are FFI library-binding inits rather than immutable data tables; both are a different shape and stay out of scope.
