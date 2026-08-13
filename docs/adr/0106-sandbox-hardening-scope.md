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
