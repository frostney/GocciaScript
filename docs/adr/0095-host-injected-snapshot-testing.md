# Keep snapshot persistence behind a test-host adapter

**Date:** 2026-07-14
**Area:** `testing`, `runtime`, `cli`
**Issue:** [#120](https://github.com/frostney/GocciaScript/issues/120)

Snapshot assertions are part of the testing runtime, but filesystem and source
rewriting operations remain host responsibilities. `TGocciaSnapshotState`
owns snapshot keys, counters, update policy, and reconciliation. It talks only
to `IGocciaSnapshotHost`, which supplies external snapshot storage and queued
inline source edits. `GocciaTestRunner` installs the file-backed host; embedders
can install another adapter or omit snapshot support.

The default formatter is a testing-specific Vitest-compatible implementation,
not the general runtime display formatter. Hosts can replace it through
`IGocciaSnapshotFormatter`, and JavaScript tests can add ordered pretty-format
plugins through `expect.addSnapshotSerializer`. This separates stable snapshot
text from console/debug formatting without hard-coding the test runner as the
only consumer.

Inline snapshots use the active native-call source location. The interpreter
records the call-expression location, while the VM records its debug location;
the file host resolves either position back to the nearest
`toMatchInlineSnapshot` call before parsing its arguments. Edits are applied in
descending source order so multiple assertions in one file remain stable. The
runner defers the physical write until every engine has finished, combining
edits process-wide so parallel tests can safely target one imported helper or
different virtual sections of the same multifile source.

Snapshot property shapes reuse the shared deep-equality path and its asymmetric
matcher values. The formatted snapshot is a merge of the received value and the
shape, so dynamic fields are persisted as matcher descriptions. This makes the
same matcher families work in `toEqual`, `toStrictEqual`, `toMatchObject`,
`toContainEqual`, `toHaveProperty`, and snapshot properties.

Consequences:

- Local runs create missing snapshots but do not silently replace mismatches or
  remove obsolete entries. CI runs neither write nor accept missing, stale, or
  obsolete snapshots. `-u`, `--update`, and `--update-snapshots` opt into all
  writes and pruning.
- External snapshots require persistent source. Existing inline snapshots can
  be checked from stdin, but stdin cannot create or update source text.
- Snapshot formatting and persistence are replaceable independently, while the
  testing API and lifecycle remain identical in interpreter and bytecode modes.
- The engine core gains only a transient call-site channel; it does not gain a
  filesystem dependency.
