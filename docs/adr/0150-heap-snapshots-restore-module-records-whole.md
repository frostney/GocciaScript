# 0150 - A heap snapshot restores module records whole, compiling each source module without linking it, and writes data modules as values

**Date:** 2026-10-19
**Area:** `embedding`, `modules`
**Related:** [ADR 0148](0148-heap-snapshots-at-idle-points.md), [ADR 0133](0133-modules-root-their-value-exports.md), [ADR 0139](0139-a-reload-links-into-the-cached-module-record.md)

## Context

[ADR 0148](0148-heap-snapshots-at-idle-points.md#modules) has the restore
link the module graph without evaluating it, then fill each linked record,
and has data modules read again and checked by their content hash. Building
it showed two problems with that:

- Linking creates each module's environment, binds its imports and
  instantiates its functions, all of which the snapshot then overwrites, and
  a link failure in the restoring engine would leave a half-linked graph in
  its loader.
- A data module's exports are ordinary values: a run can change a JSON
  module's object, and reading the file again would lose that change, which
  is the shortcut ADR 0148 rejects for source modules.

## Decision

**A snapshot holds every module record the loader caches, in a table of its
own.** A record is not a managed object. Its header (path, cache keys,
host ownership, and for a source module its source path and bytecode hash)
is read before any object is allocated; its payload (environment, export
bindings, exports table, star and ambiguous names, namespace object,
evaluation promise, asynchronous-cycle root) once every object is filled.

**A restore compiles each source module without linking or running it.** It
reads the module from the header's source path through the loader, under
the loader's read policy, compiles it as linking would, and refuses a module
whose bytecode hash differs from the snapshot's. It then fills a fresh
record from the payload, restoring the environment, export bindings and
namespace object as the run left them.

**The records join the loader only once the whole snapshot restored.** They
are cached under their keys as evaluated modules, so a later import or
`import()` finds them and runs no body. A refused restore frees them and
leaves the loader as it was; a key the loader already caches refuses the
restore.

**A data module is written as its exported values**, not read again, so the
run's changes to them survive.

## Consequences

- The restoring engine needs the same module files, and must not have loaded
  the modules the snapshot holds.
- Host and runtime-loader modules, and bindings of them, refuse the snapshot
  until host module codecs land, as do loader state outside the records
  (a module still linking, being reloaded or whose evaluation failed,
  deferred namespaces, module source objects) and `import.meta` objects.
- The entry module, evaluating while its run waits at a checkpoint, is
  restored as evaluated.
- A source module the restore compiled from its file takes the file's
  modification time, checked against the snapshot's bytecode, so a later
  import of a copied file does not evaluate it again; a data module, a
  module whose bytecode the engine had compiled before and the engine's own
  entry source keep the run's, so a newer file is loaded again, as in the
  run.
