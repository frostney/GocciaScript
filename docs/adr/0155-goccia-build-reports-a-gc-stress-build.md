# 0155 - Goccia.build reports a GC-stress build

**Date:** 2026-10-22
**Area:** `runtime`, `testing`, `gc`
**Related:** [ADR 0024](0024-platform-metadata-via-goccia-build.md), [#1679](https://github.com/frostney/GocciaScript/issues/1679)

## Context

The weekly GC-stress job now runs every `tests/` file under the GC-stress
build, which collects every 8 dispatched instructions. A collection marks the
whole live heap, so a case that builds N objects costs time proportional to N
times the heap. Seventeen files build thousands to hundreds of thousands of
objects (nested Proxies, bound-function and class chains, the deep chains
the marker tests, WeakMap chains, FinalizationRegistry registrations, long
source lists) and ran past a 15-minute cap under stress, though each passes
in seconds in an ordinary build. Their small cases are the ones most likely to catch a missing
GC root, so excluding or time-capping whole files loses coverage the job
exists to provide.

## Decision

**`Goccia.build.gcStress` is `true` in a GC-stress build and `false`
otherwise.** It is an enumerable, read-only boolean beside `os`, `arch` and
`date`, fixed at compile time by `GOCCIA_GC_STRESS`. A test reads it to shrink
a large N under stress, or to skip a case whose point is a size past an
engine bound, while its other cases still run under stress.

## Considered options

- A per-file list in `scripts/gc-stress.ts` with a short wall-clock cap,
  reported as stress-slow. Rejected: a capped file reports nothing about its
  fast cases.
- An environment variable read by the test runner. Rejected: it reaches only
  the test runner, and the build, not the environment, decides the collection
  schedule.

## Consequences

- One more compile-time property on `Goccia.build`; ordinary builds report
  `false`.
- Tests that rely on it must also guard `typeof Goccia`, as other
  `Goccia.build` users do, so they still run on other engines.
