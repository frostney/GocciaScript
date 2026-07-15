# Virtual modules use the ordinary module loader

**Date:** 2026-07-14
**Area:** `modules`, `configuration`, `embedding`, `cli`
**Related:** [ADR 0014](0014-bytecode-and-interpreter-feature-parity.md), [ADR 0063](0063-real-source-phase-imports.md), [ADR 0072](0072-import-bytes-with-immutable-backing.md), [ADR 0075](0075-shadowrealm-conformance.md)

Configuration-supplied modules are stored in a registry owned by
`TGocciaModuleLoader`. The registry resolves a configured address and supplies
its content kind and bytes; the existing loader remains responsible for
parsing, linking, evaluation, caching, import phases, attributes, and module
namespace identity. Virtual modules therefore do not have a second evaluator,
resolver, cache, or reduced proposal surface.

The configured content types are JavaScript, JSON, text, and bytes. TypeScript
is an alias for JavaScript and omission means JavaScript. JSON, text, and bytes
reuse the loader's existing synthetic-module behavior. An explicit import
attribute can reinterpret stored content through those same loaders, including
reading JavaScript source text as bytes.

Configured names are canonical module addresses. Relative names are anchored to
the config or manifest that defines them, while relative imports inside virtual
JavaScript resolve from the importing virtual address. `import.meta.url` exposes
that address and `import.meta.resolve` calls the same loader resolution seam.

Virtual definitions outrank filesystem and import-map resolution. A resolvable
filesystem collision emits a warning. Host modules and `goccia:` runtime
modules are reserved and produce a configuration error instead of being
shadowed. Definitions can be changed before first resolution but are immutable
after load so cached module identity remains stable.

ShadowRealm children copy the definition set into their own loader. This keeps
configuration read-only from the child while giving every realm independent
module records, values, evaluation, and caches.

Programmatic modules are called host modules. `RegisterHostModule` and
`RegisterHostModuleProvider` are the preferred API names; the historical
`RegisterGlobalModule` names remain compatibility aliases. Global value
injection also remains fully supported, but documentation prefers modules for
new host configuration because imports make dependencies explicit.
