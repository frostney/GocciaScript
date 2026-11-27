# 0196 - import.meta and Script imports follow the real path

**Date:** 2026-11-27
**Area:** `modules`
**Related:** [ADR 0108](0108-specifier-only-module-resolution-errors.md), [#1498](https://github.com/frostney/GocciaScript/issues/1498), [#2206](https://github.com/frostney/GocciaScript/pull/2206)

## Context

Since #2206, the loader finds a file's module record by its canonical path and module type, and resolves the
module's relative imports from the canonical directory, as Node and Bun do. Two parts still used the spelling
the module or Script entry was first reached by:

- `import.meta.url`, and the base `import.meta.resolve` resolves against. For a symlinked file in another
  directory, `import.meta.resolve("./sib.mjs")` named the link's sibling while the module's own
  `import "./sib.mjs"` loaded the real file's sibling. Which spelling `import.meta.url` showed depended on which
  import reached the module first.
- A Script entry's import declarations and its `import()` calls, and a ShadowRealm's `importValue`, which have
  no module record of their own and resolved from the path as given. A Script reached through a symlink bound
  the link directory's `./sib.mjs`, while the same file run as a Module bound the real directory's.

## Decision

A module's `import.meta` gets the address its record resolves relative imports from: a host file's canonical
path, the canonical source path or canonical-directory logical address of a bytecode file, and a virtual
module's configured address. `import.meta.resolve` still computes its result lexically against that base.

Each Script entry run gets an import base of its own: its file's real path, or for a Script compiled to bytecode
the address a bytecode module there would have. Its code's templates carry it as a negative `ModuleId`, so its
`import()` calls, its closures' in later runs included, resolve from that base, never from another Script that
has the same name. The engine keeps the bases until it releases its modules. The Script's imports module, keyed
by the entry's path plus `#script-imports`, has no file of its own: it resolves its requests from the same base
and never matches a file's record, even a file named like it.

A ShadowRealm's `importValue` resolves from its caller's base, found by the caller's module record or Script,
which name it in whichever realm the import runs. Only code known by nothing but its source name, such as eval
code, resolves from the real path of the module or file that name gives.

Diagnostics and stack traces keep the spelling the module was first loaded under. Sandbox mode has no host
paths to canonicalize and is unchanged.

## Consequences

- `import.meta.url` names `/private/var/...` rather than `/var/...` on macOS, Windows long names rather than
  8.3 short names, and a symlink's target. A script can therefore learn where a symlink in its import graph
  points, as it can under Node. This is not an error message and is outside ADR 0108.
- `new URL("./x", import.meta.url)` and `import.meta.resolve("./x")` agree with `import "./x"`.
- A Script entry reached through a symlink reads its imports from the real directory, so the read capability
  must cover that directory, as it already had to for the same file run as a Module.
