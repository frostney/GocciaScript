# 0106 - Scope the sandbox-hardening programme to finding bugs over jailing them

**Date:** 2026-08-11
**Area:** `sandbox`

## Context

A hardening programme was proposed in six work packages: a fuzzing and
memory-safety harness, fetch DNS pinning and address policy, an allocator-level
memory budget, a watchdog interrupt for cooperative timeouts, an out-of-process
sandbox execution mode with per-platform OS jails, and capability metering with
an effect log.

Validating each package against the tree first changed three of them
materially, because the proposal consistently under-credited infrastructure
that [ADR 0103](0103-layered-untrusted-execution-boundaries.md) had already
landed:

- The response-body cap it proposed to add already existed as an 8 MiB
  constant enforced across all three body paths. The work was making it
  configurable, not adding it.
- `--max-memory` was described as sampled and outrunnable. It is not: it is
  charged at allocation sites and checked before allocating. The real gap was
  narrower — native growth points with no owning value, where a single
  `new Array(100000000)` overshot a 64 MiB budget by 23x.
- The structured capability trace it proposed to build already exists as the
  capability audit event, with six kinds, a JSONL sink, and its own document.

The threat model was left implicit throughout, and settling it is what
determined the rest. Three adversaries were distinguished: source that is
merely careless, source that is adversarial because a prompt-injection payload
steered it, and an attacker hunting memory-safety bugs in the engine itself to
escape. The engine is a from-scratch, manually memory-managed implementation of
roughly 275k lines, much of it agent-written, so the third adversary is not
hypothetical — an agent-written codebase carries a higher prior on latent
memory-safety defects than a hand-audited one.

## Decision

Target careless and injection-steered source directly, and answer the
memory-safety adversary primarily by **finding** defects rather than by
containing them.

- **Fuzzing is the programme's centre, not its safety net.** If the concern is
  defects we do not know about, discovering them dominates jailing their
  consequences. `GocciaFuzzHarness` drives one input through lex, parse, and
  both executors under tight bounds; every engine-modelled outcome exits zero
  so that only an unmodelled fault is a finding. It denies all module loads,
  because a fuzz input must not reach the host filesystem.
- **Fetch resolves once, validates the resolved address, and pins the connect
  to it,** re-applied per redirect hop. ADR 0103 rechecked the allowlist per
  hop but matched on hostname while the connect re-resolved independently,
  leaving a check-then-use window. TLS continues to verify against the
  hostname: pinning decides which address is dialled, never which identity the
  peer must prove.
- **Memory budget gating covers native growth points.** Sites with an owning
  value keep charging and releasing; sites without one are gated — checked
  before allocating, never charged. This is a deliberate asymmetry, recorded
  because it is easy to misread the budget as stronger than it is: gating
  bounds any single allocation but not the aggregate of many small ones.
  *(This sentence understates the gap by enough to mislead. See
  [Amendment 1](#amendment-1--the-aggregate-caveat-measured) below for what it
  costs in measured bytes.)*
- **No watchdog thread.** The cooperative-timeout gap is a chokepoint-coverage
  problem, not an architecture problem. Blocking waits already poll every
  iteration, so the only uncovered case is a native loop with no poll point —
  which an audit and a test fixture catch at the source, where a watchdog would
  catch it only at runtime, at the cost of a thread per armed timeout and
  arm/disarm races against FPC threadvars.
- **Out-of-process isolation stops at process separation.** Phase A defines the
  protocol and Phase B adds `--isolate=process` with a parent-enforced deadline
  and hard kill. Per-platform syscall jails are deferred with the matrix
  recorded. Process separation buys crash containment and turns a wedged native
  loop into `kill()`; a syscall jail only constrains what an already-escaped
  attacker does next, which a deploying host can obtain today by running the
  process in a container. Deferring the jails also avoids shipping three of
  four platform implementations that cannot be executed by their author.
- **Capability budgets are metered at the capability-audit emission site.** That
  boundary already observes every capability crossing with a kind and subject,
  so budgets and the audit trail are driven by the same event and cannot
  disagree about what happened.
- **A module load with no provider configured throws a plain `Error` carrying a
  code.** ECMA-262 sec-HostLoadImportedModule (ES2026, §16.2.1.10) requires a
  throw completion but mandates no type. For this exact case the major engines
  agree on `Error`: V8 rejects with `Not supported` when no host import
  callback is set, JavaScriptCore's default loader with `Could not open the
  module`, and SpiderMonkey with `Module load hook not set`. `TypeError` is the
  convention for a *configured* loader that failed (HTML, Deno), which is a
  different condition and should stay distinguishable. The error carries `code`
  and nothing else: unlike the sandbox filesystem errors of
  [ADR 0092](0092-sandbox-filesystem-error-contract.md), whose `path` is a VFS
  address the guest itself named, the only address available at this refusal is
  the resolver's output — a host filesystem path by default — and the engine
  that has no provider is precisely the one running untrusted source. It also
  bounds the contract: resolution runs before retrieval, so a specifier that
  never resolves fails earlier and carries no code at all.

## Consequences

The memory budget now refuses allocations it previously permitted, which is a
default-on behaviour change. It is accepted deliberately: a ceiling that
silently permits a 23x overshoot is not behaviour worth preserving
compatibility with, and unlike the fetch address policy there is no legitimate
use case on the other side. The fetch policy stays opt-in for exactly that
reason, so the two differ by intent rather than by oversight.

Private-range denial reduces SSRF but does not eliminate it: a public address
that proxies to an internal one remains reachable. Address classification is
deny-biased, so shortened and hex forms are refused rather than reinterpreted.

In-process execution keeps cooperative timeouts. A native loop that fails to
poll can still overshoot its deadline, and out-of-process execution is the
answer for hosts that cannot accept that. This is stated plainly rather than
implied, consistent with [VISION.md](../../VISION.md): the sandbox remains a
reduced attack surface, not a verified security boundary, and nothing here
changes that.

"Effect log" is rejected as a term. It named the same concept as capability
audit event and would have split one vocabulary in two; the existing entry is
extended to cover delivery in the sandbox run result instead.

## Amendment 1 — the aggregate caveat, measured

**Date:** 2026-08-11

Decision records here are immutable; this is an exception on the narrow ground
that the original text is not merely incomplete but materially misleading. It
reads as a theoretical caveat. It is a measured ~15x overshoot, and a reader
sizing a budget from the sentence above would size it wrong. The decision
itself is unchanged — gate, do not charge — so this corrects the statement of
its consequence rather than the choice.

### What was measured

`GocciaScriptLoaderBare --max-memory=67108864` (64 MiB) on macOS aarch64,
against the gated build, peak RSS from `/usr/bin/time -l`:

| Workload (same ~4.8M total properties) | Outcome | Peak RSS |
| --- | --- | --- |
| 40,000 objects x 120 properties | completes | 978 MB |
| 4,000 objects x 1,200 properties | completes | 1007 MB |
| 1 object x 4.8M properties | **refused** at 37,748,640 bytes | 324 MB |

A one-line restructuring of a runaway script — spreading the same properties
over more objects — turns a refusal into a ~15x overshoot that runs to
completion. The first workload never consults the gate at all; the second
consults it on every doubling and it permits every time.

### Why

Two independent reasons, and only the second one matters.

1. **A band of small maps is never checked.** `GATED_GROWTH_MIN_BYTES` (4096)
   suppresses reports whose transient footprint is below it, so with
   `SizeOf(TEntry) = 24` on a 64-bit target an object that never exceeds 62
   properties is never checked, and the bucket array is not checked below 1024
   buckets (about 358 entries). This is a real blind spot and it is now pinned
   by a unit test, but it is not the cause.

2. **The remaining budget the gate compares against omits the dominant cost.**
   `TGocciaPropertyDescriptor`
   (`source/units/Goccia.Values.ObjectPropertyDescriptor.pas`) is a plain
   class, not a `TGCManagedObject`, so descriptors never enter
   `GC.BytesAllocated`. The gate therefore weighs a script-sized block against
   a budget that always looks nearly empty, and permits. The single-map case
   is refused only because *one reallocation's* transient reached 37 MB
   against a 64 MiB ceiling — the request size did the work, not the
   accounting.

   The size of the omission is bounded by the run below: 4,000 objects of 120
   properties each completes under a **16 MiB** budget, and every one of those
   objects reports a 4512-byte entry-array growth that is permitted. Permission
   requires `BytesAllocated + 4512 <= 16777216`, so `BytesAllocated` was still
   under 16 MiB with roughly 480,000 descriptors live and 131 MB resident.

Rebuilding with `GATED_GROWTH_MIN_BYTES = 64`, which reports essentially every
growth, moved the two distributed workloads to 975 MB and 1000 MB: unchanged
inside run-to-run noise.
Lowering the threshold cannot close a hole that lives in the used-figure, so it
was left at 4096 rather than churned for no measured gain. (A property-write
microbenchmark showed no cost either way, so the constant is not being defended
on performance grounds — it simply buys nothing.)

### Standing statement

The budget bounds **the size of any one gated allocation**, not the engine's
resident memory. Objects below the small-map band are not gated at all, and no
number of gated objects sums to a refusal. Charging descriptors would fix this
and is out of scope for this programme; until then, a host that needs a real
ceiling must impose one outside the process (`ulimit`, cgroup, container, or
`--isolate=process`). This is consistent with
[VISION.md](../../VISION.md): a reduced attack surface, not a verified
boundary.

`scripts/test-cli-apps.ts` pins both halves — the refusal cases assert peak
RSS, and a distributed-shape case asserts that it still completes far past its
budget — so if the aggregate hole is ever closed, that test fails and this
amendment has to be revisited rather than quietly rotting.
