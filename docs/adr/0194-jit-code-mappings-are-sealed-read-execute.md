# 0194 - JIT code mappings are sealed read-execute

**Date:** 2026-11-27
**Area:** `engine`, `jit`
**Related:** [ADR 0186](0186-g2-serializes-every-core-before-new-jit-code-runs.md)

## Context

On Linux x86-64, the baseline and optimizing tiers used a bump allocator backed by
pages with read, write and execute permissions. An embedder can require writable and
executable memory to be mutually exclusive. Linux may also add execute permission
when a mapping requests `PROT_READ` under `READ_IMPLIES_EXEC`.

## Decision

`JitPublish` copies generated code into a private anonymous mapping requested with
`PROT_WRITE`. It changes the mapping to `PROT_READ | PROT_EXEC` and performs the
core serialization from ADR 0186 before exposing an entry point. Generated code
is immutable after publication. The engine thread tracks its code mappings and
unmaps them at teardown. The fixed shared stubs live until the process ends.

If mapping, sealing or recording a mapping fails, publication returns no entry.
The existing execution tier or interpreter continues handling that code.

## Consequences

- Every publication takes a mapping and permission change. Short bodies can use
  more mapped pages than a shared bump allocator.
- The Linux engine gate checks actual allocation and sealing syscalls, private
  read-execute permissions on a compiled function, and mapping removal across
  consecutive engine lifetimes.
