# 0133 - A module roots the values it holds directly

**Date:** 2026-10-09
**Area:** `gc`, `modules`, `sandbox`
**Related:** [ADR 0105](0105-argument-collections-root-their-elements.md)

## Context

A `TGocciaModule` holds its exports in three shapes. A local export is a binding
in the module's environment scope, and `SetEnvironment` roots that scope. A
forwarded export belongs to its source module. A value export
(`AddExportValue`, `UpdateExportValue`) stores the value itself. Next to the
bindings, the export table (`ExportsTable`) keeps a value per name: the value of
a value export, the snapshot a local export took when it was linked, or, for the
YAML, TOML, JSON5 and indexed-data modules, the only record of the export,
written there directly. Nothing rooted the value exports or the table. The only
path that marked them was the module namespace object, which is created only
when something asks for the namespace.

The sandbox's `fs` and `goccia` modules and the JSON, text and bytes modules
publish through `AddExportValue`; the data modules above write the table. A
default or named import such as `import fs from "fs"` never builds a namespace
object, so a collection between the module's creation and the importer reading
it freed the exported value while the module still handed it out. The entry's
import check (`ValidateStaticNamedImports` → `CanResolveExport`) then ran a type
test on the freed object. Under a small `--max-memory` a single run faults; in a
host that reuses one process for many scripts, the extra live heap from earlier
runs moves the first collection early enough that an ordinary run faults. A
local export's snapshot had the same gap: once the binding was reassigned, the
old value could be freed while the table still named it, and a namespace object
built afterwards marked the freed object.

## Decision

A module is a garbage-collection root source for the values it holds directly.
`TGocciaModule` owns a `TGocciaModuleRoots` (a `TGCRootSource`) for its
lifetime; on every collection it marks the module's value-bound exports, its
export table and its evaluation promise. It deliberately does not read local
bindings: their environment scope is a root of its own, and reading a binding in
its temporal dead zone would raise inside the collector.

The root is tied to the module rather than to each value. `AddRootObject` keeps
a set, not a count, so rooting each exported value would let the first module
freed unroot a value that another module still exports.

## Consequences

- A value in a module's bindings or export table lives as long as the module,
  with or without a namespace object. That includes a local export's linking
  snapshot after the binding has been reassigned: the table keeps naming it, so
  it is kept until the module is freed.
- Freeing a module releases its values at the next collection.
  `Goccia.Modules.ExportRoots.Test` checks both directions, for value exports,
  direct table writes and replaced snapshots, with a probe value that records
  its own destruction.
- Each module adds one entry to the collector's root-source list and a walk of
  its export maps per collection; a program has few modules, so the cost is
  negligible next to marking the heap.
