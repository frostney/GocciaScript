# 0139 - A reload links its new version into the cached module record

**Date:** 2026-10-14
**Area:** `modules`, `gc`
**Related:** [ADR 0133](0133-modules-root-their-value-exports.md), [ADR 0136](0136-module-function-declarations-are-compiled-while-linking.md)

## Context

The module loader reloads a cached module when its content provider reports a
newer modification time (`TGocciaModuleLoader.CheckForModuleReload`). It used
to load the new version as a second `TGocciaModule`, copy that record's export
bindings into the cached record, and free the second record. Importers and
namespace objects hold the cached record, so copying kept them working, but the
freed record was still in use:

- The reload is linked inside the request that found the module in the cache,
  so it is evaluated later, by that request. The load state the evaluation
  reads, and the evaluation context in it, named the freed record. The bytecode
  VM writes each `export` it runs into the context's module, so the first
  reloaded version with a local export wrote into freed memory (#1647).
- A module linked for the first time during the reload, which imports the
  reloading module back, was bound to the freed record.
- The cached record took the new version's environment scope as its own, then
  freeing the second record retracted that scope's collector root.
  `AddRootObject` keeps a set, not a count (ADR 0133), so nothing rooted the
  environment the cached record now read its exports from.

Every reload also evaluates a module whose namespace object is already alive
while its exports are uninitialized. Marking a namespace object read each
export's value, and reading a binding in its temporal dead zone raises, so a
collection during the reload's evaluation threw a `ReferenceError` out of the
collector. A first load in an import cycle, or a module that imports its own
namespace, could hit the same throw.

## Decision

A JavaScript module keeps one record for as long as its loader caches it. A
reload links and evaluates the new version into that record:

- `CheckForModuleReload` names the cached record as the reload target for its
  cache key, and `InstantiateModule` links the new version into it instead of
  creating a record. Every reference the reload creates, from the load state,
  the evaluation context and the modules it links, names the cached record.
- `TGocciaModule.BeginReload` sets the record's exports, environment,
  evaluation promise and modification time aside, and leaves the record empty
  for the new version. The record's root source marks what was set aside, and
  the old environment stays registered as a root until the reload ends.
- When the new version has linked, `CommitReload` releases what was set aside
  and retracts the old environment's root. When it fails to link,
  `RollbackReload` puts the previous version back and the loader caches the
  record again, still older than its source, so the next request retries.

A data module (JSON, text, bytes, or a format a runtime extension loads) is
still built as a new record, because extensions create their own records.
Nothing but the reload refers to that record, and it holds only values, so
`TGocciaModule.TakeExportsFrom` moves its bindings and table into the cached
record before the loader frees it. The move transfers the environment's root
rather than sharing it.

Marking an export binding no longer reads it. A local binding marks its
environment, a value binding its value, and a forwarded binding nothing, since
its source module's roots mark what it forwards.

## Consequences

- `LoadModule` returns the same record before and after any number of reloads.
  Importers, namespace objects and code that captured the record read the
  newest version.
- Functions, classes and objects an earlier version created keep reading the
  environment they were created in. Once nothing references an old
  environment, the collector frees it with everything created in it.
- A version that fails to link leaves the previous version in place. A version
  that links and then throws while evaluating fails the way a first load does:
  the loader records the error and retires the record.
- The engine keeps every version's compiled code for its lifetime, as it does
  for eval and `Function` code (ADR 0134), so each reload still adds that code
  to the engine's memory.
- `Goccia.Modules.Reload.Test` reloads modules repeatedly and collects
  garbage after every reload. It covers importers, old closures and
  class instances, changed exports, a module linked during a reload that
  imports the reloading module, a reload that fails to link, JSON modules, and
  the live object count across 200 reloads.
  `tests/language/modules/namespace-collection-during-evaluation.js` collects
  while a module that imports its own namespace is still evaluating.
