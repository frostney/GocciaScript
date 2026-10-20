# 0153 - Precompiled modules use the module linker

**Date:** 2026-10-20
**Area:** `bytecode`, `modules`, `embedding`
**Related:** [ADR 0005](0005-register-based-bytecode.md), [ADR 0140](0140-an-entry-with-module-syntax-runs-as-a-module.md)

## Context

An embedder can keep compiled libraries in memory, but the public bytecode
loader accepts files and imports still parse source. Reading a file through
many small stream operations also consumes much of the front-end saving.
Running imported artifacts through the entry executor would bypass the
module loader's linking, live bindings, cycles and asynchronous evaluation.

The existing format also drops BigInt constants: the writer emits the tag
without its payload, and the reader adds no constant. In smol-toml 1.8.0,
this shifts later constant indexes and the verifier correctly rejects the
result. The format preserves `StrictThis` but loses `StrictCode`, and does
not identify the language profile under which the parser and compiler ran.

## Decision

**Format 96 records the compilation profile and the module's execution
layout.** Compatibility flags and preprocessors have explicit wire bit
assignments; strict types and the settled script/module goal are recorded
separately. Effective parser switches are normalized before recording.
Loading for execution requires equality with the host's profile. A detected
entry adopts the recorded goal; an explicitly chosen conflicting goal
fails. Unsupported-feature warning recovery is recorded because it changes
the compiled meaning of unsupported forms. Optimizer and reporting options
that do not change language semantics are excluded.

**The codec accepts a buffer and verifies it before returning a module.**
File loading reads a bounded buffer and uses the same path. A borrowed
pointer is consumed synchronously, and returned modules retain no pointers
into it. Registration stores an owned copy. Buffer lengths, expanded table
allocations, string lengths, enum tags, nesting and trailing data remain
input-parser boundaries on both 32-bit and 64-bit targets.

**Imported artifacts use the existing module linker.** The compiler records
a declaration plan containing binding kinds, ordered module requests and
export mappings. It compiles the body with the same global-backed scope and
preinitialized function settings as an imported source module. The loader
materializes declaration nodes from the verified plan and supplies the
already compiled body to its existing linking and evaluation machinery.
Those declaration nodes are never parsed, compiled or evaluated as code.
Deferred graph walks use the same source-or-bytecode acquisition path.

This preserves the loader's implementation of ECMA-262 2026
[InitializeEnvironment, §16.2.1.7.3.1](https://tc39.es/ecma262/2026/multipage/ecmascript-language-scripts-and-modules.html#sec-source-text-module-record-initialize-environment):
module functions are instantiated while linking, before dependency bodies
run. Templates record their module binding names. Entry completion values
remain separate from promises that govern module evaluation.

**Resolution chooses content without changing module identity.** An embedder
registers bytecode under the address imports resolve to. That address, rather
than the artifact's original compiler path, governs relative imports and
`import.meta`. The virtual registry retains its collision, normalization and
replacement rules. File companions use the bundler's existing convention,
replacing the source extension with `.gbc`. Disk preference is opt-in through
`PreferBytecode` / `--prefer-bytecode`; explicit `.gbc` imports select bytecode
directly. Absence permits source fallback. Corruption or a profile mismatch
does not.

Companions participate in the resolver's extension, directory-index, alias
and package candidate order. Physical containment and pinned-content checks
apply to the artifact. When source is absent, a bounded verified metadata
read recovers its source extension without trusting its original directory;
`import.meta.resolve` can therefore validate an artifact. The loader retains
that decoded module for loading and maps each physical companion to one
logical address, keeping explicit and extensionless imports in one graph.

**Source-phase imports still require source.** They expose a source object;
a compiled representation alone cannot supply the original source text.
An artifact may contain a source-phase request, which follows the existing
source-loading path.

**Fix the codec rather than weakening verification.** BigInt payloads are
canonical signed decimal strings, validated with a linear scan. Every wire
constant retains its slot; duplicate encodings that would collapse interned
slots are rejected. Strictness and function metadata needed at runtime are
serialized with the new format.

## Considered options

- Run imports with the entry executor. Rejected because it bypasses module
  initialization, cycles and deferred evaluation.
- Embed source and reparse declarations at load time. Rejected because it
  keeps the front-end work that AOT loading is intended to remove.
- Infer declarations from instructions. Rejected because export mappings,
  import phases and declaration kinds are semantic inputs to linking.
- Always prefer disk companions. Rejected because a stale artifact could
  silently replace source during ordinary development.
- Accept a compatible subset of profile flags. Rejected because disabling
  a parser feature does not establish equivalent runtime semantics.

## Consequences

- Existing artifacts must be rebuilt; the loader rejects earlier versions.
- Embedders can ship bytecode without source for evaluation-phase imports.
- Hosts must compile and execute with matching language options.
- Bytecode is still untrusted input, not proof that compiler invariants hold.
- Source maps and call-site diagnostic descriptors have their existing
  limitations; the declaration plan does not reconstruct original source
  positions.
