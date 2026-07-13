# Unified performance barometer with a frozen JetStream subset

**Date:** 2026-07-12
**Area:** `benchmarking`, `ci`, `website`
**Issue:** [#858](https://github.com/frostney/GocciaScript/issues/858)
**Related:** [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md), [ADR 0090](0090-web-tooling-goccia-benchmark-lane.md)

GocciaScript will add a JetStream 3 reference lane and present its retained
main-branch history together with AWFY on one public Performance Barometer. The
barometer treats QuickJS and Node.js as reference engines: they provide scale and
direction, but their product goals are not assumed to match GocciaScript's and
the dashboard is not a ranking.

The JetStream input is a frozen, manifest-owned subset of six pure-JavaScript
workloads from pinned JetStream 3.0 commit
`3967678fa8ab98d847ab33cf3728dba726fa854b`. Upstream workload files remain
unchanged. A generic shell adapter supplies only timing, deterministic randomness
where required, and result serialization. Workloads that require Wasm, DOM,
workers, host filesystem access, duplicate the Web Tooling lane, or do not fit
the execution budget are outside this lane. The reduced three-iteration protocol
with five process repetitions preserves JetStream's First/Worst/Average score
shape but is explicitly not the official full-suite JetStream score.

AWFY and JetStream keep their native measurement units in retained reports. The
dashboard exposes one directional reference-ratio convention: `1.00×` means
aligned performance and values above one mean GocciaScript was proportionally
slower. AWFY calculates `Goccia time / reference time`; JetStream calculates
`reference score / Goccia score`. QuickJS and Node.js ratios remain separate.

Retained history is segmented by its measurement contract. Corpus commit,
selected subset, driver version, QuickJS version, or Node.js version changes
break trend lines rather than connecting unlike points. Failed reports remain
structurally publishable and visible as gaps. When the newest run is incomplete,
the last complete result is labeled stale instead of being presented as current.
AWFY and JetStream use separate Blob namespaces while the website reads both into
the unified `/performance` route.

Consequences:

- JetStream expands the external performance barometer without making its full
  browser harness or web APIs compatibility requirements for GocciaScript.
- The public dashboard communicates direction and reproducibility, not a winner
  or an absolute performance claim.
- Measurement-version changes and failures are visible parts of the historical
  record instead of being smoothed over.
- Web Tooling stays a separate Goccia-only viability lane.
