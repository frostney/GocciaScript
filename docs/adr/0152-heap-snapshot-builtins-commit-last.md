# 0152 - A heap restore commits built-in differences after every fallible step

**Date:** 2026-10-20
**Area:** `embedding`, `heap snapshots`
**Related:** [ADR 0148](0148-heap-snapshots-at-idle-points.md)

## Context

ADR 0148 applies changed built-ins before filling the restored object graph.
A later malformed payload, exhausted budget or failed allocation would then
refuse the restore after changing the destination engine's built-ins.
Publishing other restored state, such as module records, can also allocate
and poll execution limits. Completing the object reader alone does not
establish that restoration succeeded.

## Decision

**Prepare built-in differences separately and commit them last.** The reader
validates the graph, roots and differences, then builds replacement property
and element stores under the memory ceiling and execution limits. Registered
symbols, and any module records, must be published before built-in state
changes. Fallible publication must roll back its own changes on failure.
Only after those steps
succeed does the reader install every prepared difference in a final pass
that allocates nothing, polls nothing and calls no guest code.

Object and module filling must not depend on a changed built-in's own state.
A future codec that needs that state must read the pending representation or
revisit this decision; it must not apply differences early.

This replaces ADR 0148's ordering of built-in differences. Differences still
preserve final key order, descriptors, prototypes, intrinsic properties and
integrity. Their temporary stores are owned by the reader until commitment;
a refusal frees them without applying any difference.

## Consequences

A refused restore applies no built-in difference, including when publication
of other restored state exhausts the instruction budget. The final pass cannot stop
between two built-ins, so its work is limited to installing state that the
polled preparation already bounded and charged.

Resolving names can still construct lazy built-ins. Restore into a fresh
engine: refusal does not rewind lazy initialization or compiled source.
