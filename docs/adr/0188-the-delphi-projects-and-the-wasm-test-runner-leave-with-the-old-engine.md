# 0188 - The Delphi projects and the Wasm test runner leave with the old engine

**Date:** 2026-11-24
**Area:** `build`, `apps`, `g2`
**Related:** [ADR 0006](0006-delphi-mode-performance-parity.md)

## Context

G2 replaces the old engine as GocciaScript's only engine, and the
applications in `source/app` now build on it: `build.pas` compiles each one,
runs it to snapshot the engine's compiled preludes, and compiles it again with
the snapshot.

Two programs existed only on the old engine:

- The Delphi projects (`GocciaScript.Delphi.groupproj` and one `.dproj` per
  application) built the old engine's applications with Delphi 12. G2 builds
  with FreePascal 3.3.1 only: its interpreter and baseline JIT rely on FPC's
  native-TLS threadvars, and its build snapshots preludes through `build.pas`.
  The plan for the switch puts FreePascal first and Delphi in a later release.
- `GocciaWasmTestRunner` was a single-threaded, manifest-driven test runner
  meant to compile under the Lakon Object Pascal compiler for a WebAssembly
  host. Lakon has reached its lexer milestone and compiles no program yet, so
  no build of the runner ever ran on a Wasm host; natively it duplicated
  `GocciaTestRunner`. G2 runs each test file on a thread of its own.

## Decision

The Delphi projects and `GocciaWasmTestRunner` are removed with the old
engine's applications. `build.pas` no longer has a `wasmtestrunner` target,
and the documentation states that the applications build with FreePascal.

The `LAKON` conditionals in the shared units stay: they cost nothing and keep
those units ready for a Wasm build.

## Consequences

- Delphi support returns when G2 builds under Delphi; that work starts from
  G2's sources, not from the removed projects.
- A Wasm test lane starts from G2 once Lakon compiles programs. G2's
  thread-per-file test runner needs a single-threaded mode there, which the
  removed runner's manifest protocol (`PASS`/`FAIL`/`SUMMARY` lines) can
  serve as the model for.
- `scripts/test-cli-permissions.ts` no longer checks the Wasm runner's `-P`
  handling.
