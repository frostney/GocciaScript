# Engine-owned host environment

**Date:** 2026-07-14
**Area:** `engine`
**Issue:** [#881](https://github.com/frostney/GocciaScript/issues/881)

JavaScript-observable time, time zone, and randomness flow through one
`TGocciaHostEnvironment` owned by each engine. Its small interface hides the
system and injected clock/RNG adapters used by `Math.random`, `Date.now`,
`Temporal.Now`, implicit `Intl.DateTimeFormat` time, and `performance`.
Infrastructure timing remains on `TimingUtils`, so execution timeouts,
profiling, garbage collection, benchmark measurement, and Test262 agent
coordination continue advancing in deterministic mode.

The `--deterministic` CLI flag and `"deterministic": true` config key install
one fixed profile: Unix epoch and monotonic time `0`, time zone `UTC`, and a
portable SplitMix64 stream seeded with unsigned 64-bit `0`. Child realms share
the clock and derive stable, distinct RNG streams. Hosts can instead inject
`IGocciaHostClock` and `IGocciaHostRandom` adapters through
`Engine.HostEnvironment`; configuration must happen before execution and
before runtime extensions such as `performance` attach.

`GocciaScriptLoader --host-environment=<module>` provides the same injection
seam to JavaScript callers. The module exports callable `epochNanoseconds`,
`monotonicNanoseconds`, `timeZoneIdentifier`, and `random` providers and is
evaluated before the loader runtime profile attaches, so `performance` captures
the injected origin. Default system providers preserve each consumer's prior
time-zone fallback; an explicitly configured provider overrides it.

This keeps the state per engine rather than global, follows per-engine realm
isolation, and gives callers one seam for all host-controlled nondeterminism.
It implements ECMA-262 2026 `Math.random` (§21.3.2.28,
`sec-math.random`) and `Date.now` (§21.4.3.1, `sec-date.now`) against snapshot
`0248456c758431e4bb8e5d26333ff1865123c9cd`; Temporal follows
`HostSystemUTCEpochNanoseconds` and `SystemTimeZoneIdentifier` from the current
Temporal proposal.
