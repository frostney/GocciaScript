# Reject broader property inline caches without AWFY transfer

**Date:** 2026-07-06
**Area:** `bytecode`, `performance`
**Related:** [ADR 0065](0065-shape-lite-inline-cache-design.md), [ADR 0076](0076-same-runner-benchmark-comparison.md), [ADR 0081](0081-reject-value-caches-for-allocation-reduction.md), [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md)

GocciaScript will not merge the July 2026 broader property-inline-cache spike
for constant property access. The existing shape-lite own-property read cache
remains in place, but the attempted expansion to broader read-side inline
caches is rejected because it did not transfer from targeted probes to real
AWFY rows. The project should not accept extra VM cache complexity when the best
evidence is "same or slightly worse" on object-heavy programs.

The investigated lane started from a real hotspot: targeted probes showed
`propaccess-monomorphic`, `prop-polymorphic`, and `prop-megamorphic` far behind
QuickJS, and object-heavy AWFY rows such as `Richards`, `DeltaBlue`, `Bounce`,
and `Storage` were likely affected. The spike tested broader own-property
read-side caching and a constant-key write cache while preserving ECMAScript
semantics for accessors, proxies, deletion, prototype mutation, and descriptor
changes.

The combined implementation was unstable as a candidate. An initial interleaved
10x AWFY run showed useful wins on `Richards`, `Bounce`, and `Storage`, but
`DeltaBlue` regressed from 5.330 ms to 5.824 ms (**-9.27%**) with high
coefficient of variation. A later 5x run still had `DeltaBlue` at **-1.87%**.
A 30x rerun moved the combined patch positive on the selected rows, but with
high variance on several rows, so the result was not strong enough to justify
the broader cache shape by itself.

Splitting the experiment identified the important boundary. The read-PIC-only
variant produced a synthetic probe win on `prop-polymorphic`, but did not
improve real AWFY rows:

| Variant | Target | Baseline median | Candidate median | Result | Baseline CV | Candidate CV |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| read-PIC-only, 20x AWFY | `Richards` | 755.451 ms | 760.829 ms | **-0.71%** | 2.09% | 1.43% |
| read-PIC-only, 20x AWFY | `DeltaBlue` | 3.421 ms | 3.453 ms | **-0.91%** | 9.03% | 9.47% |
| read-PIC-only, 20x AWFY | `Bounce` | 16.164 ms | 16.086 ms | **+0.48%** | 2.19% | 1.68% |
| read-PIC-only, 20x AWFY | `Storage` | 24.842 ms | 25.186 ms | **-1.38%** | 7.39% | 3.94% |
| read-PIC-only, 20x probe | `propaccess-monomorphic` | 54.129 ms | 54.225 ms | **-0.18%** | 10.02% | 3.19% |
| read-PIC-only, 20x probe | `prop-polymorphic` | 68.912 ms | 62.118 ms | **+9.86%** | 1.74% | 3.32% |
| read-PIC-only, 20x probe | `prop-megamorphic` | 68.937 ms | 69.538 ms | **-0.87%** | 1.31% | 1.54% |
| read-PIC-only, 20x probe | `method-call-fixed-arg` | 151.401 ms | 149.293 ms | **+1.39%** | 2.74% | 3.46% |

That is the rejection reason: the read-side cache improved the synthetic
polymorphic probe but was flat-to-negative on the AWFY programs it needed to
help. This matches earlier rejected optimization lanes such as string interning
and broad value caches: a local microbenchmark win is insufficient when the
real workload result is neutral, negative, or too noisy for the complexity.

A narrower write-IC-only split looked more promising, but it is not accepted by
this ADR. It should be treated as a separate future candidate with its own
implementation, semantic tests, and acceptance gate:

| Variant | Target | Baseline median | Candidate median | Result | Baseline CV | Candidate CV |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| write-IC-only, 30x AWFY | `Richards` | 739.781 ms | 680.861 ms | **+7.96%** | 1.24% | 0.84% |
| write-IC-only, 30x AWFY | `DeltaBlue` | 3.374 ms | 3.289 ms | **+2.52%** | 11.70% | 4.74% |
| write-IC-only, 30x AWFY | `Bounce` | 16.056 ms | 15.189 ms | **+5.40%** | 3.25% | 3.05% |
| write-IC-only, 30x AWFY | `Storage` | 24.792 ms | 24.025 ms | **+3.09%** | 2.14% | 2.28% |
| write-IC-only, 20x probe | `propaccess-monomorphic` | 53.629 ms | 53.054 ms | **+1.07%** | 1.65% | 1.61% |
| write-IC-only, 20x probe | `prop-polymorphic` | 68.291 ms | 68.490 ms | **-0.29%** | 13.39% | 1.53% |
| write-IC-only, 20x probe | `prop-megamorphic` | 69.008 ms | 69.102 ms | **-0.14%** | 2.23% | 1.47% |
| write-IC-only, 20x probe | `method-call-fixed-arg` | 149.220 ms | 149.153 ms | **+0.05%** | 1.35% | 1.13% |

Future property-access optimization work should follow this rule:

- split read ICs, write ICs, method-load cooperation, and computed-key caches
  into independently measured patches;
- require interleaved `--goccia-baseline` / `--goccia-candidate` AWFY runs, not
  sequential batches;
- require targeted probes to transfer to object-heavy AWFY rows before adding
  VM cache state;
- preserve the existing ECMAScript semantic exclusions for proxies, accessors,
  private fields, descriptor changes, deletion, and prototype mutation;
- treat probe-only wins as diagnostics, not merge criteria.

The practical decision is to keep the existing monomorphic shape-lite read cache
and reject the broader read-side PIC expansion until it shows real AWFY transfer.
The write-only result can be reconsidered later, but only as a smaller,
separately validated performance change.
