# es-toolkit Validation

*A bounded, pinned public-package probe lane for library compatibility evidence.*

## Executive Summary

- **Pinned input** — The manifest records es-toolkit 1.49.0's release commit and npm tarball SHA-512 integrity
- **Exact evidence** — Named public-package probes run in interpreter and bytecode modes; the lane does not claim whole-suite coverage or publish a pass-rate percentage
- **Separate classifications** — Semantic failures, bytecode divergence, disabled capabilities, host globals, and harness failures remain distinct in the JSON report
- **Sandbox-aligned** — The lane does not enable unsafe dynamic code generation or add Node/browser host stubs

## Scope and pin

The validation lane exercises named public-library behaviors against the
published es-toolkit 1.49.0 ESM artifact. The manifest at
`scripts/es_toolkit_harness/manifest.json` pins both the upstream release commit
and the npm tarball integrity. When `--package-dir` is not provided, the runner
downloads that exact tarball, verifies its SHA-512 integrity, and extracts it
into a temporary directory.

This is deliberately a probe lane, not a reconstruction of es-toolkit's Vitest
suite. Its JSON report contains exact named outcomes and makes no whole-suite
pass-rate claim. The durable probes cover the public package behaviors that
were independently reproduced:

- combining-mark deburring;
- ordinal word matching;
- a true invariant;
- empty `unzipWith`;
- sorted-index behavior at the maximum array-length boundary.

Internal functions and unreproduced audit cases are not promoted into the
durable surface merely to increase the probe count.

## Running the lane

```bash
./build.pas loader
python3 scripts/run_es_toolkit_validation.py \
  --output=tmp/es-toolkit-validation.json
```

Pass `--package-dir=/path/to/package` to use an already extracted es-toolkit npm
package. The runner still checks the package name and version against the
manifest.

Pass `--tarball-cache=/path/to/es-toolkit.tgz` to reuse the pinned archive. The
runner verifies the cached bytes against the manifest before extraction and
populates a missing cache only after a successful integrity check.

PR CI runs the lane in the Linux `cli` job and retains the normalized JSON
report. Main CI does the same on the x86-64 Linux leg. Both workflows share a
cache key derived from the pinned integrity value.

## Classification contract

| Classification | Meaning |
|---|---|
| semantic pass / failure | Both execution modes completed the same named public-library probe; a failure is an assertion mismatch in both modes |
| bytecode divergence | Interpreter and bytecode outcomes differ; known transitive-module TDZ failures are accepted only when they match the manifest's pinned error pattern |
| disabled capability | The default-disabled `Function` constructor is recorded as sandbox policy, not library semantics |
| host global | `Buffer`, `process`, and `Blob` availability is recorded without treating Node or browser host APIs as ECMAScript conformance |
| harness failure | Download/integrity, package, process, timeout, marker, or native-runtime failures made the evidence untrustworthy |

Known bytecode divergences remain visible in the report and are allowlisted by
their exact error class and message shape, not by a blanket skip. A newly passing
bytecode probe is accepted; a different divergence fails the lane.

## Vision boundary

The runner enables only the compatibility syntax and semantics required by the
published library. It does not enable the unsafe Function constructor and does
not install `Buffer`, `process`, or `Blob` stubs. This keeps the evidence aligned
with [Vision](../VISION.md): Node host compatibility is not a goal, broader web
host APIs remain a product boundary, and dynamic code generation stays disabled
by default.
