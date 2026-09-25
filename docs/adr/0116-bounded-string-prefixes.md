# Bounded string prefixes with reserved materialization capacity

**Date:** 2026-09-05
**Area:** `runtime`, `bytecode`, `gc`

Bytecode string accumulators retain an immutable prefix and a flat suffix once
the prefix reaches 256 UTF-16 code units. Every concatenation creates a distinct
runtime value. A chain has at most 32 links; a native `Value` read or the next
append at the depth limit materializes it with one allocation and a backward
copy of the chunks. This reduces repeated copying without changing saved
aliases, UTF-16 contents, or object coercion order.

The append reserves both the eventual flat buffer and the currently retained
suffix. Materialization consumes already reserved capacity, releases the suffix
charge, and drops its prefix link. It never triggers GC or invokes guest code.
That is a correctness requirement: callers such as primitive comparisons can
hold unrelated operands only in native locals across a `Value` read. An initial
candidate that reserved during the read was rejected because protecting the
string being flattened did not protect those other temporaries.

The collector traces each prefix link. Before an append reservation needs a
collection to fit, it flattens the parent so the collector can discard unaliased
intermediate nodes. Aliases remain valid even when an ancestor is materialized
before its descendants. Deferred values belong to the executing runtime; literal
constants, hint strings, and prototype initialization use ordinary flat strings,
whose reads do not mutate their representation.

## Evidence

The baseline was `f33d9c6a061e6cb61dce65ffcf8514da1c051870`, built with
FreePascal 3.2.2 and `./build.pas --prod loader`. Measurements used macOS 26.5.2
on arm64, bytecode mode, and a 256 MiB GC budget. Each order discarded one
warmup per binary and retained seven interleaved samples, followed by the
reverse order. Timings below are median engine execution milliseconds; process
CPU timings confirmed the direction despite host scheduling noise.

| Workload | Baseline AB / BA | Candidate AB / BA |
|---|---:|---:|
| `string-append-30k`, 8,000 iterations | 473.3 / 461.9 | 25.3 / 25.9 |
| JetStream Base64 worker kernel | 1432.0 / 1432.7 | 1111.3 / 1112.3 |

For the append workload, execution ranges were 442–563 ms on the baseline and
24–29 ms on the candidate. Base64 ranges were 1233–1819 ms and 997–1409 ms.
The Base64 kernel came from JetStream revision
`c603c04db8505477867974a69789309ded2cc948`,
`worker/bomb-subtests/string-base64.js`. Its body was unchanged, with deterministic
input and worker-completion shims around it. Its round-trip check and an
independently computed checksum passed. This is a standalone kernel result, not
a JetStream suite score.

AWFY Json at revision `74306fec151070fd07157cefeacf19e7e0bcdc89` guarded short
strings and parser workloads. Initial order-dependent results prompted a longer
confirmation: seven ABBA blocks, five verified parses per invocation, and 14
measured samples per binary. CPU medians were 1383 ms and 1318 ms, with overlapping
ranges; there was no repeatable guard regression. No Json speedup is claimed.

A three-second baseline host sample of the 30,000-iteration append workload
placed 1550 of 2446 top-of-stack samples in `memmove`, reached through
`fpc_unicodestr_concat` in VM dispatch. The 8,000-iteration VM profile recorded
24,102 `OP_ADD` instructions. This identifies repeated payload copying as the
target rather than reducing the number of runtime string objects.

## Limits

The bounded chain reduces copy frequency; it does not make arbitrary repeated
appends asymptotically linear. Frequent content reads force materialization.
Reserved capacity remains charged even before the flat buffer exists: the
8,000-iteration probe retained an approximately 240 MiB GC-accounted peak and
seven collections in both builds, while the native heap allocation reported at
the end of execution fell from approximately 109 MiB to 5 MiB. These counters
describe different resources and should not be substituted for each other.

This is not content-keyed string interning or a cache of runtime values.
[ADR 0013](0013-reject-string-interning.md) and
[ADR 0081](0081-reject-value-caches-for-allocation-reduction.md) remain in force.
