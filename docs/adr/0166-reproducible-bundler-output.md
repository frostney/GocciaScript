# 0166 - Bundler names and output destinations are deterministic

**Date:** 2026-10-28
**Area:** `compiler`, `bytecode`, `cli`
**Related:** [ADR 0153](0153-precompiled-modules-use-the-module-linker.md), [#1957](https://github.com/frostney/GocciaScript/issues/1957), [#1958](https://github.com/frostney/GocciaScript/issues/1958), [#1959](https://github.com/frostney/GocciaScript/issues/1959)

## Context

Private-name and eval-environment counters persist across compilations on a
worker thread. Computed class-element locals include the AST node's address.
Both reach serialized bytecode, making identical inputs differ with worker
history and process allocation. Resetting counters alone is insufficient:
instance private storage uses the compiled label, so independently compiled
classes and repeated evaluations of one class body can alias storage.

Directory output also discards relative paths. Different source extensions
or separate input roots can select the same artifact, and parallel workers
can write that artifact concurrently.

## Decision

The compilation root owns generated-name counters shared by its descendant
scopes. Computed-element locals use the deterministic class prefix and element
index. A direct eval reserves the hidden environment names and private prefixes
imported from its caller before allocating new ones. Byte identity applies to
unchanged source paths, contents, compiler version and compilation options.

Compiled private labels identify lexical declarations, not runtime storage.
The VM resolves them through the declaring class and uses its runtime identity
for instance and stamped-object storage, as it already does for static fields.
A stamped receiver retains the identity owner for as long as its private brand
exists, including when initialization throws; a collected class address cannot
be recycled into an existing brand ([#1972](https://github.com/frostney/GocciaScript/issues/1972)).
Private methods and accessors resolve through the declaring class rather than
selecting a same-named element from a derived class. This implements the fresh
private names required for each evaluation by
[ECMA-262 2026, ClassDefinitionEvaluation §15.7.14](https://tc39.es/ecma262/2026/multipage/ecmascript-language-functions-and-classes.html#sec-runtime-semantics-classdefinitionevaluation).

The bundler expands the complete invocation into an output plan before writing.
Two planned writes to the same destination are a usage error naming both inputs
and the destination; existing artifacts are retained. Extension replacement
remains unchanged because companion loading resolves `library.mjs` to
`library.gbc`. Each directory argument contributes its relative tree below an
existing output directory. Explicit file arguments still contribute basenames.

Nested parent creation preserves config output confinement. Confined paths use
the existing pinned root and platform checks; unconfined CLI destinations keep
their existing authority. Workers consume the validated plan without changing
its input roots or output paths.

## Consequences

Bytecode no longer depends on scheduling, thread history or AST addresses.
Runtime class evaluations remain isolated even when their deterministic labels
match. The artifact format and companion lookup convention stay unchanged.

Previously successful invocations that silently overwrote outputs now fail
before emitting artifacts. This preflight is not an atomic build transaction:
parse, compilation or filesystem errors after planning may still leave outputs
from other successfully compiled inputs.
