# 0151 - A deterministic sandbox run stamps its files with the deterministic clock

**Date:** 2026-10-20
**Area:** `sandbox`, `embedding`
**Issue:** [#1528](https://github.com/frostney/GocciaScript/issues/1528)
**Related:** [ADR 0093](0093-engine-owned-host-environment.md), [ADR 0139](0139-a-reload-links-into-the-cached-module-record.md)

## Context

The deterministic host profile freezes the time a script reads: `Date.now()`,
`Temporal.Now` and `performance.now()` all report the epoch. The sandbox
virtual filesystem kept its own clock. `TSandboxVirtualFileSystem` accepts an
injected `TSandboxFsClock` and reads the wall clock without one, and neither
`TGocciaSandboxContext` nor `TGocciaSandboxHost` passed one. So under
`--deterministic` every file a sandbox run held, the copied inputs included,
carried the real time: a script could read the wall clock through
`fs.statSync(path).mtimeMs`, and two runs of the same script printed different
times and wrote different `metadataChanges` into their JSON diffs.

The clock has to exist before the filesystem: the root directory is stamped
when the filesystem is created, and an application copies its inputs in
before it creates the first engine. An engine's host environment is
configured only after that, and each nested run has an engine of its own.

## Decision

**An embedder gives the sandbox context or host its clock when it creates
it.** `TGocciaSandboxContext.Create` and `TGocciaSandboxHost.Create` take an
optional `AClock: TSandboxFsClock` (nil reads the wall clock, as before) and
pass it to the filesystem, so the root directory and every input carry it.
`CaptureBaseline` forks the filesystem, and a fork keeps the clock.
`TGocciaSandboxContext.Clock` returns it. An isolated nested run's context
(`runScript` with `{ sandbox: true }`, or `goccia --sandbox` in the shell) is
created with its parent's clock, as it inherits its parent's quotas.

**The clock is not owned.** As with the filesystem's own parameter, the caller
frees it after the context or host, and so after every nested context that
was given it.

**A clock can follow a host clock.** `TGocciaSandboxFsHostClock` adapts an
`IGocciaHostClock`: `Now` is the host clock's epoch time, converted by
`SandboxUnixNanosecondsToDateTime`. `TGocciaHostEnvironment.DeterministicClock`
returns the clock the deterministic profile installs, which
`UseDeterministicProfile` now uses too, so the profile's instant is defined
once.

**`GocciaRunner`'s sandbox mode uses it under `--deterministic`.** The runner
creates the host with a `TGocciaSandboxFsHostClock` over the deterministic
clock, so every access, modification, change and birth time in the run is
the epoch: `atimeMs`, `mtimeMs`, `ctimeMs` and `birthtimeMs` are `0`, and the
`Date` accessors read 1970-01-01T00:00:00.000Z. The JSON diff still lists
every content change (`create`, `modify`, `delete`, `mkdir`, `replace`),
which compares bytes, not times; its `metadataChanges` is empty, because
no timestamp moves. The runner is the only application that runs a sandbox;
the test runner has no sandbox mode.

## Considered options

- **An advancing deterministic clock** (a counter stepped on every read).
  Timestamps would keep their order, so metadata changes and the module
  loader's modification-time reload would still be seen. Rejected: the
  profile freezes time everywhere else, and a file would then read a time
  that `Date.now()` never reports. Whether `performance.now()` should advance
  under the profile is a separate question in #1528; a file clock can follow
  that answer through the same adapter.
- **Reading each run's engine host environment when a file is written.** The
  filesystem outlives its engines and is written before the first one
  exists, and a nested run shares or copies it, so there is no one engine to
  read. Rejected.
- **The context owns the clock.** It would differ from the filesystem's
  parameter, and a nested context given the same clock would have to know it
  must not free it. Rejected.

## Consequences

- A deterministic sandbox run's file metadata no longer depends on when it
  ran, so two runs print the same times and write the same diff.
- The module loader reloads a cached module when its file reports a newer
  modification time ([ADR 0139](0139-a-reload-links-into-the-cached-module-record.md)),
  and retries a failed import once its file is newer. Under a frozen clock
  no file is ever newer, so in a deterministic sandbox run a module rewritten
  during the run is not reloaded by a later import, and a failed import is
  not retried after its file is rewritten.
- The shell's `ls -l` lists every file at `1970-01-01 00:00`.
- Without `--deterministic`, and for an embedder that passes no clock,
  nothing changes.
