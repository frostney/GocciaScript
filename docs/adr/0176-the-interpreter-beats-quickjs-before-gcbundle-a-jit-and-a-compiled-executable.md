# 0176 - Performance is a goal: the interpreter beats QuickJS first, then .gcbundle, an opt-in JIT and a compiled executable

**Date:** 2026-11-05
**Area:** `performance`, `bytecode VM`, `benchmarking`, `website`
**Issue:** [#2064](https://github.com/frostney/GocciaScript/issues/2064)
**Supersedes:** the throughput stance in [`VISION.md`](../../VISION.md) ("Not performance-competitive with V8/SpiderMonkey … over raw throughput"); in part [ADR 0076](0076-same-runner-benchmark-comparison.md) and [ADR 0091](0091-unified-performance-barometer.md)
**Related:** [ADR 0087](0087-awfy-cross-engine-benchmark-methodology.md), [ADR 0138](0138-bytecode-is-the-only-way-gocciascript-executes.md), [#2066](https://github.com/frostney/GocciaScript/issues/2066), [#862](https://github.com/frostney/GocciaScript/issues/862), [#822](https://github.com/frostney/GocciaScript/issues/822), [#863](https://github.com/frostney/GocciaScript/issues/863), [#2102](https://github.com/frostney/GocciaScript/issues/2102), [#2070](https://github.com/frostney/GocciaScript/issues/2070)

## Context

GocciaScript is primarily an engine for embedding. A host that wants an
interpreter-class JavaScript engine compares it with QuickJS first, so the speed
of the interpreter is part of what the engine offers, not a side effect of it.

The project has said otherwise until now. `VISION.md` lists GocciaScript as
"not performance-competitive with V8/SpiderMonkey" and says it "prioritizes
correctness, embeddability, and reduced attack surface over raw throughput".
ADR 0076 keeps the PR benchmark comparison advisory partly because `VISION.md`
deprioritizes raw throughput. ADR 0091 treats QuickJS and Node.js only as
reference engines and says the Performance Barometer is "not a ranking" and
shows "direction and reproducibility, not a winner". The repo's
`optimize-runtime` skill aims each optimization wave at 0.6×–0.8× of QuickJS
speed and still describes a tree-walk interpreter that ADR 0138 removed. Work
aimed at those bars stops short of QuickJS, and a dashboard framed as "not a
ranking" cannot show a target being met.

The gap at the time of this decision, from CI run
[41824992451](https://github.com/frostney/GocciaScript/actions/runs/41824992451)
on `main` at `ffd9e5b4` (ubuntu-latest x64, QuickJS 2026-06-04):

- **AWFY:** the geomean of Goccia time / QuickJS time over the 14 AWFY
  benchmarks is **4.65×** (one cold iteration per process, AWFY corpus
  `74306fec`).
- **JetStream:** the geomean of QuickJS score / Goccia score over the six-workload
  JetStream 3 subset is **11.0×** (JetStream corpus `c603c04d`).

## Decision

Performance is a project goal, with four targets in this order of priority.

### Terms

- **Interpreter** means the Goccia bytecode VM with no JIT tier. Since ADR 0138
  it is the only way GocciaScript executes; the word does not refer to the
  removed tree-walk interpreter.
- A measurement uses the **default production build**: the release binary CI
  builds for that target, run with the flags the lane's driver passes for the
  upstream corpus (the compatibility flags it needs) and no
  performance-specific options.

### Target 1: the interpreter is faster than QuickJS (0.21.0)

Target 1 is met for a commit when, on every platform GocciaScript ships
prebuilt binaries for, both of these geomeans are below **1.00×**:

- AWFY: Goccia time / QuickJS time (`geomeanRatios.goccia_over_qjs` in the
  AWFY report);
- the JetStream subset: QuickJS score / Goccia score
  (`geomeanRatios.goccia_over_qjs` in the JetStream report).

The **measurement contract** is the Performance Barometer's: the AWFY lane
(`scripts/awfy-ci-report.js` with `perf/awfy/manifest.json`, ADR 0087) and the
frozen JetStream subset (`scripts/jetstream-ci-report.js` with
`perf/jetstream/manifest.json`, ADR 0091). GocciaScript and QuickJS samples are
interleaved by repetition on one machine per workload (CI runs each JetStream
workload on its own runner). Every workload completes and passes its result
check where it has one: AWFY's `verifyResult`, and JetStream's upstream
`validate()`, which four of the six subset workloads lack and so run
execution-only.

The target is judged on the contract current at the time. When the contract
changes, for example to the warm AWFY protocol of #2066 or a new corpus pin, a
result from an older segment does not count as meeting the target under the new
one. The barometer keeps breaking its trend lines as ADR 0091 says. Only the
judgement of target 1 uses a narrower set of fields: the corpus, the subset, the
driver, the QuickJS version, and the default production build with the driver's
flags (see Terms). A Node.js version change alone does not move target 1 to a
new segment, because Node.js is not a target-1 comparator.

**QuickJS is the comparator for target 1**, at the version
`.github/scripts/install-quickjs.sh` pins, built on the measuring machine.
Node.js with its JIT stays on the barometer as an outside reference. #2066
adds interpreter-class comparators, such as `node --jitless` and JavaScriptCore
with its JIT off, as context; target 1 does not require beating them.

The shipped platforms are the six release targets that
`.github/scripts/package-release-archive.sh` packages: `aarch64-darwin`,
`x86_64-darwin`, `x86_64-linux`, `aarch64-linux`, `x86_64-win64` and
`i386-win32`. The goal is the same on every one of them. CI publishes the
barometer from ubuntu-latest x64 only. How the other five are measured is
**deferred to [#2102](https://github.com/frostney/GocciaScript/issues/2102)**
(0.21.0): how QuickJS is built on Windows, whether `i386-win32` is compared
against a 32-bit QuickJS, whether a dispatchable native-runner workflow or
maintainer hardware produces the reports, and whether emulated results count.
This ADR does not settle those questions.

Target 1 is a milestone acceptance criterion, not a per-PR merge gate.

### Later targets, in order

- **Target 2, `.gcbundle` bytecode artifacts (0.22.0):** a multi-module
  bytecode format that the VM loads natively, so a host can ship compiled code
  instead of source.
- **Target 3, an opt-in baseline JIT (1.1.0):** a template JIT modeled on
  [frostney/wasmlight](https://github.com/frostney/wasmlight)'s, compiling
  bytecode in a single pass to fixed machine-code templates over the
  in-memory register file, for aarch64 and x86-64 on 64-bit UNIX hosts. It is
  off unless the host enables it, and it is checked differentially against
  the interpreter, which stays the tier of record. Windows, 32-bit targets and
  iOS stay bytecode-only. Whether the JIT backend is shared with wasmlight
  through lwpt or copied into GocciaScript is left open, to be decided when
  1.1.0 starts.
- **Target 4, a compiled executable (1.2.0):** host-target executable
  packaging, with signing and notarization, and an interpreter-free native
  executable compiled ahead of time. iOS stays bytecode-only here as well.

1.0 requires targets 1 and 2, alongside a stable C ABI and public API and 100%
of test262. The JIT and the compiled executable are not 1.0 requirements: they
are 1.x work, milestones 1.1.0 (target 3) and 1.2.0 (target 4). Milestones 0.21.0
and 0.22.0 carry targets 1 and 2.

### What stays from ADR 0076 and ADR 0091

- ADR 0076's same-runner PR comparison stays, and so does its advisory,
  comment-only status. Its other reasons, the measured noise floor and test262
  as the gating regression check, still hold; the `VISION.md` throughput
  stance is no longer one of them.
- ADR 0091's lanes, frozen JetStream subset, ratio convention (`1.00×` is
  parity, above it GocciaScript is slower), measurement-contract segmentation,
  visible failed-run gaps and stale labeling stay. What changes is the
  framing: for target 1, QuickJS is the bar the barometer measures progress
  against, not only a reference for scale, and the barometer may say whether
  that bar is met.

## Consequences

- The default goal of an optimization wave is target 1: both geomeans below
  `1.00×` against QuickJS, not 0.6×–0.8× of QuickJS speed. The
  `optimize-runtime` skill says so and describes the single bytecode VM.
- The Performance Barometer copy and the glossary's **Performance Barometer**
  entry name QuickJS as the target-1 bar. Figures on the website stay
  live-sourced from the retained reports.
- 0.21.0 needs measurements for five more platforms. Until #2102 settles the
  method, target 1 can be tracked on ubuntu-latest x64 only, and it is met only
  once every shipped platform is measured.
- A JIT-enabled engine, such as Node.js or V8, is not a target. The JIT that
  target 3 adds is opt-in, so the interpreter's own speed remains what every
  host gets by default, including iOS and Windows hosts.
- The rewrite of `VISION.md` and `CONTEXT.md` in #2070 cites this record for
  performance. The throughput work itself is tracked by #862 and #822 in
  0.21.0.
- This ADR settles the technique and the go decision that #863 asks for: a
  template JIT modeled on wasmlight, opt-in. #863 is re-scoped to the wasmlight
  template design and the question of sharing the backend through lwpt, in
  1.1.0.
