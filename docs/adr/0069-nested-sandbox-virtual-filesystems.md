# 0069 - Nested sandbox virtual filesystems

## Status

Accepted.

## Context

ADR 0068 introduced `GocciaSandboxRunner` with explicit seed baselines and shared nested execution: `runScript(path)` and shell `goccia path` execute another entry path against the same virtual filesystem. That is useful for orchestration, but it does not cover agent workflows that need to run a child program against a disposable filesystem derived from the parent sandbox.

The nested child must not read host paths. At this point in execution, the trusted import boundary is already the parent virtual filesystem. Child population therefore needs to seed from parent sandbox paths or inline child-only data.

## Decision

Keep shared nested execution as the default for compatibility:

- `runScript(path)` runs against the current sandbox filesystem.
- `goccia path` runs against the current sandbox filesystem.

Add explicit child sandbox execution:

- `runScript(path, { sandbox: true, seed, diff, diffFormat })`
- `goccia --sandbox --seed <from[=to]> --diff --diff-format json|unified path`

Child seed entries are copied from the parent virtual filesystem into a new child `TSandboxVirtualFileSystem`. A string seed copies that parent path to the same child path. `{ from, to }` copies a parent file or directory to a child target. Inline `{ path, text }` and `{ path, base64 }` entries define child-only files.

The runner captures a child baseline after seeding. Child writes are discarded with the child context unless the caller requests a diff. `runScript(..., { diff: true })` returns the child diff on the result object. Shell `goccia --diff` appends the child diff to command stdout.

Child execution uses the same engine configuration and executor mode as the parent sandbox runner invocation, including interpreter versus bytecode mode and configured network allowlists.

## Consequences

Nested execution now supports both orchestration styles:

- Shared mode for workflows that intentionally mutate the parent sandbox.
- Child mode for isolated experiments, checks, generators, and agent tool runs that should produce inspectable changes without committing them to the parent VFS.

Seed semantics stay consistent with the top-level runner: seeds are import baselines, not mounts. The only difference is source authority: top-level seeds copy from host paths, while nested child seeds copy from parent sandbox paths.

The `"goccia"` module grows an options object, but no new globals are introduced and the ordinary `GocciaScriptLoader` runtime surface remains unchanged.
