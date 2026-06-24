# 0071 - Reject symlinks in sandbox seed imports

## Status

Accepted.

## Context

ADR 0068 established that `GocciaSandboxRunner` seeds are import baselines, not mounts: `--seed` and `--seed-config` `from` paths copy host data into the virtual filesystem before execution, and the host path never becomes a live mount. The `TSandboxVirtualFileSystem` itself deliberately does not support symlinks (see its unit header), so the sandbox namespace has no way to address the host.

The seed importer undermined that boundary. It read host files through `TFileStream`, which dereferences symlinks. A symlink encountered during import — the literal path passed to `--seed`, or any descendant inside a seeded directory — therefore copied the bytes of its *target* into the sandbox VFS. Because a symlink target can point anywhere on the host, a seed could pull data from outside the seed root into the sandbox without that ever being visible at the import site. This contradicts the sandbox-first, reduced-attack-surface posture in `VISION.md`, where the sandbox is meant to have no ambient host filesystem access.

There is a genuine trade-off. The alternative is to give the VFS first-class symlink nodes plus path-resolution semantics, so a seeded symlink is preserved as a symlink rather than dereferenced. That is a larger surface — link nodes, resolution, loop detection, and a re-examination of the chroot-jail invariant — and it widens rather than reduces what the sandbox can express. The seed import contract from ADR 0068 only promises a baseline copy, so dereferencing was never a guaranteed behavior to preserve.

## Decision

Reject host symlinks during `--seed` and `--seed-config` host imports: the seed path's own leaf (the final component the host named) and every descendant discovered while walking a seeded directory. A cross-platform check (`HostPathIsSymlink` in the shared `FileUtils` unit; POSIX `lstat` + `S_ISLNK`, Windows reparse points / junctions via `faSymLink`) inspects each host node before it is read. The seed path's trailing separator is stripped before the check, because POSIX `lstat` follows a final symlink when the path ends in `/` — otherwise `--seed=linkdir` would be rejected while `--seed=linkdir/` was dereferenced.

When a symlink is found, the import fails closed: the run aborts with exit code 1 and emits the error, naming the offending path:

```text
Seed path is a symlink (not supported): <path>
```

No VFS symlink node is added, and no bytes are copied. Symlink preservation — giving the VFS first-class symlink nodes with their own resolution semantics — is intentionally out of scope for this decision.

Two cases are deliberately not covered. Symlinks that appear as **ancestor** directory components of a host-supplied seed path (for example `--seed=/a/linkdir/file`, where `linkdir` is a symlink above the named leaf) are not rejected: an ancestor is the host's explicit choice of where the seed lives, indistinguishable from benign system symlinks such as macOS `/var` → `/private/var` or `/tmp` → `/private/tmp`, so rejecting it would refuse ordinary temp paths. The threat this decision addresses is a symlink **at or inside** the requested seed root, not the location the host points at. Full real-path containment (canonicalizing the resolved path and confirming it stays within an intended root) is a larger, separate policy and is also out of scope.

## Consequences

A previously-working seed that relied on a symlink being dereferenced now errors instead of silently copying the link target. This is user-visible and intentional: the failure names the path so the cause is obvious, and the fix is to seed the real file or directory directly.

On Windows, junctions and other reparse points are treated as symlinks and rejected on the same path.

The guard lives in the shared `FileUtils` unit, so host-path symlink detection is a single cross-platform implementation reused by the runner rather than per-call-site logic.

In-VFS operations and nested `runScript` semantics are unchanged. Nested child seeds copy from the parent virtual filesystem rather than the host (ADR 0069), so they never re-enter the host import path and are unaffected by this guard.
