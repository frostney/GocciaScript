# 0144 - Read-only paths are enforced by the virtual filesystem, and refused writes are EROFS

**Date:** 2026-10-17
**Area:** `sandbox`
**Issue:** [#1546](https://github.com/frostney/GocciaScript/issues/1546)
**Related:** [ADR 0092](0092-sandbox-filesystem-error-contract.md), [ADR 0119](0119-host-applied-sandbox-write-back.md)

## Context

A host that copies a dataset into a sandbox and wants the guest to treat it
as read-only had no way to say so. The guest could write, append to, delete
or rename anything in the virtual filesystem. The `--copy` inputs that are
read-only for [write-back](0119-host-applied-sandbox-write-back.md) are only
read-only to the host: the guest's write succeeds, the rest of the run sees
it, the diff lists it, and write-back skips it afterwards without telling the
script. An embedder that hands a script a read-only dataset next to a
writable workspace wants the write to fail where it happens, the way a write
to a read-only mount fails in Node.

The guest reaches the filesystem through several paths: the synchronous,
callback and promise forms of `fs`, the `$` shell, and nested `runScript`
runs that share the filesystem.

## Decision

**`TSandboxVirtualFileSystem` keeps the read-only marks and checks them in
each of its mutating operations.** `AddReadOnlyPath(path)` marks a canonical
path and everything under it, whether or not it exists yet, and
`IsReadOnly(path)` answers whether a path is covered. Opening for writing,
truncating, writing through a handle, creating a file or directory (including
the missing parts of a recursive `mkdir`), touching, deleting, and moving or
copying onto such a path raise `ESandboxFsReadOnly`, with the message
`<canonical path>: read-only file system`. So do deleting or moving a path
from inside a marked tree, deleting or moving a writable directory that holds
a marked one, which would otherwise take the marked tree with it, and moving
or copying a tree to a destination with a marked path below it, which would
otherwise create entries under the mark. Reads and copies out of a marked
tree are not affected. A fork keeps the marks. Marks cannot be removed.

Every guest write path already ends in one of those operations, so checking
there covers all of them, including forms added later. Checking in the `fs`
module instead would have to be repeated in the shell and in each new entry
point.

**A refused write is `EROFS`.** `Goccia.Sandbox.FileSystemErrors` maps
`ESandboxFsReadOnly` to code `EROFS`, description `read-only file system`,
and errno `-EROFS` (libuv's `-4043` on Windows), following
[ADR 0092](0092-sandbox-filesystem-error-contract.md). A script catches an
`Error` with `code === "EROFS"` from every `fs` form. The shell's commands
fail with exit code 1 and the message on standard error.

**The marks belong to one filesystem.** A `runScript` child that shares the
filesystem is refused like its caller. An isolated child (`{ sandbox: true }`)
gets a new filesystem of copies without marks: its writes never reach the
caller's tree.

## Consequences

- An embedder marks paths after filling the filesystem, because the marks
  also refuse the embedder's own writes.
- Error precedence follows from where each check sits. Writing a new file
  under a marked directory whose parent is missing is `EROFS`, not `ENOENT`.
  A plain `mkdir` of an existing marked directory is still `EEXIST`, because
  it would create nothing.
- `GocciaRunner` does not mark anything yet. A command-line or config opt-in
  that makes `--copy` inputs read-only at write time is left to #1546.
