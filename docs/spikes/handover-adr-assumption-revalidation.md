# Handover: ADR assumption re-validation

Pick-up brief for deciding which performance ADRs need new decision records
because their **premises** aimed at the wrong counterfactual for a QuickJS-scale
goal. Background:
[performance-simplification-audit.md](performance-simplification-audit.md)
§ Assumption validity.

## Executive Summary

- Do **not** rewrite old ADRs (append-only). Re-validation means new ADRs that
  reframe or supersede premises — or an explicit “still stands” note in a new
  ADR that cites the barometer evidence.
- Three clusters need human/product attention: **0081** (caches vs boxing
  diagnosis), **0088** (read-IC class vs property storage), **0020+0014**
  (semantic parity vs shared representation forever).
- Tactic bans still stand: interning (0013), value caches (0081), that read-PIC
  (0088), that fixed-arity collection (0089). Stop generalizing them to “area
  closed.”
- Measurement ADRs (0076, 0087, 0091) and numeric ADRs (0100, 0101) do not need
  assumption re-litigation.
- This lane owns **decisions and ADR text**. Runtime spikes belong to the
  sibling performance handover.

## Mission

Produce zero or more new ADRs (and, if needed, a short VISION or architecture
clarification) so future performance work cannot misread 0081/0088/0020 as
forbidding the only levers that could approach QuickJS.

## Start here (read order)

1. This handover + audit spike § “Assumption validity”
2. [VISION.md](../../VISION.md) — sandbox/embeddability over V8-class throughput
3. [CONTEXT.md](../../CONTEXT.md) — Performance Barometer vocabulary
4. Full texts: ADR **0013, 0014, 0020, 0065, 0066, 0081, 0088, 0089, 0091,
   0100, 0101**
5. [architecture.md](../architecture.md) — shared value substrate claim
6. Performance handover for what engineering will attempt once decisions land

## Decisions required (ask a human)

Answer these explicitly before writing ADRs. Default “no decision” blocks
Track B in the performance handover.

### D1 — Shared representation vs semantic parity

**Question:** Must interpreter and bytecode share one heap value/object
*representation* forever, or is **semantic** parity enough (same observable
behavior, denser bytecode-only layout allowed)?

- **If shared representation forever:** document that 0020/0014 imply a
  performance ceiling vs tagged-value engines; performance lane is limited to
  write-IC, proof-backed calls, and non-representation work.
- **If semantic parity only:** new ADR carving that 0020’s unified hierarchy
  does not forbid bytecode-local unboxed slots / compact objects, with test
  obligations for parity.

### D2 — How to read ADR 0081 going forward

**Question:** Confirm the intended lasting rule is “no content/range value
*caches*,” not “boxing is not a bottleneck.”

- Likely new ADR (or short superseding note ADR): boxing/representation remains
  an open lever; caches remain rejected; allocate-count is not a success metric.

### D3 — How to read ADR 0088 going forward

**Question:** Confirm lasting rule is “no broader *read* PIC without AWFY
transfer,” not “property access is finished.”

- Likely new ADR: property **storage/layout** and **write-IC** are open
  experiment classes; read-PIC expansion stays rejected until new transfer
  evidence.

### D4 — Barometer vs product ambition

**Question:** Is “get much closer to QuickJS on AWFY/JetStream” an accepted
engineering objective under VISION (reference barometer), or only “don’t get
worse / improve when cheap”?

- VISION already rejects V8/SM competitiveness. QuickJS-as-north-star is
  consistent with ADR 0091 only if humans say so out loud — otherwise the
  performance handover’s mission must be downscoped.

## What does *not* need a new ADR

| ADR | Why leave it alone |
| --- | --- |
| 0013 reject string interning | Tactic still wrong; optional later ADR only if landing short-string/`grkString` (different mechanism) |
| 0005 register VM | Still right |
| 0074 deferred call frames | Still right |
| 0076 / 0087 / 0091 | Methodology still right; 0091 pin SHA drift is editorial, not a new decision |
| 0089 defer fixed-arity | Rejection of *that design* stands; open doors already written — no reopen |
| 0100 / 0101 | Fresh; premises narrow and measured |
| 0001 / 0080 | Already superseded (0016→0100; 0098) |

## Suggested ADR outcomes (draft only after D1–D4)

Use the lightweight format in `.agents/skills/domain-modeling/ADR-FORMAT.md`
(or project equivalent). One decision per file. Do not edit 0081/0088/0020 in
place.

Possible titles (illustrative):

1. `0102 — Semantic parity does not require identical value representation`
   (only if D1 allows bytecode-local denser layout)
2. `0103 — Value caches remain rejected; unboxed representation remains open`
   (D2)
3. `0104 — Property storage and write-IC are open; broader read PIC stays closed`
   (D3)
4. Optional: barometer ambition clarification if D4 changes how 0091 is used

Also fix remaining **doc drift** without rewriting ADRs:

- ADR 0016 text still describes lifetime FPU mask — leave immutable; ensure
  `embedding.md` / `value-system.md` point at 0100 (embedding already fixed on
  this branch).
- ADR 0091 JetStream pin SHA vs `perf/jetstream/manifest.json` — note in a new
  ADR only if the pin bump policy itself changes; otherwise update non-ADR docs.

## Process

1. Resolve D1–D4 with the human (this handover’s first deliverable is answers,
   not prose).
2. If performance spikes are needed to inform D1, coordinate with the
   performance handover — prefer a time-boxed bytecode-only prototype over
   speculation.
3. Draft ADRs; link the audit spike as evidence, not as a decision.
4. Update `docs/adr/README.md` index; link from architecture/bytecode-vm only
   with one-liners.
5. Hand cleared Track B scope back to the performance lane.

## Non-goals

- Quietly reopening 0013/0081/0088 tactic bans without new measurements
- Rewriting historical ADR bodies to match 2026 reality
- Turning the public barometer into a product ranking (CONTEXT / 0091)
- Using this lane to merge interpreter and bytecode into one executor

## Done looks like

- Written answers to D1–D4
- Zero or more new ADRs merged (or an explicit “no new ADR; performance
  downscoped” record)
- Performance handover unblocked with a crisp allowed/forbidden list
