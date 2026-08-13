# 0107 - Out-of-process sandbox execution protocol

**Date:** 2026-08-11
**Area:** `sandbox`

## Context

[ADR 0106](0106-sandbox-hardening-scope.md) decided that out-of-process
isolation **stops at process separation**. It split the work into two phases:
Phase A defines the protocol, and Phase B adds `--isolate=process` with a
parent-enforced deadline and a hard kill. Per-platform syscall jails are
deferred, with the platform matrix recorded but not implemented. Fetch is not
brokered, because there is no committed Phase C jail for it to sit behind.

This ADR is Phase A: the detailed design under 0106. It defines the process
model, the wire protocol, the message schema, the failure-taxonomy mapping, and
the deferred jail matrix. It deliberately ships **no** process-spawning code, no
frame reader/writer, and no `--isolate` wiring. Phase B is a separate lane that
depends on this ADR being reviewed first, and a Phase B implementer reading only
this document must not have to invent any undocumented wire-format decision.

The host↔sandbox contract already exists as a serializable message shape, which
is what makes an out-of-process split tractable. The dedicated sandbox runner
([ADR 0068](0068-goccia-sandbox-runner.md)) is seeded from an explicit
[seed baseline](../../CONTEXT.md) and reports a **filesystem diff** rather than
writing host paths, so nothing about a run needs live host access once the seeds
are materialised. The run inputs — seed specs, run options, and the engine
option set — and the run outputs — captured output, result value, diff, and a
failure classification — are all already values, not host handles.

`TGocciaSandboxRunResult` carries a failure taxonomy — `sfkNone`,
`sfkScriptError`, `sfkResourceLimit`, `sfkTimeout`, `sfkHostError`, and
`sfkChildProcessCrash` — with `sfkChildProcessCrash` reserved for exactly this
out-of-process work. That reserved value is the seam this protocol fills in: it
is the one classification a child cannot report about itself, because a process
cannot narrate its own death.

## Decision

Define a **child-per-run**, **stdio, length-prefixed-frame** protocol that
carries one run request from parent to child and one run result from child to
parent, versioned by exact match, with the crash classification set only by the
parent from out-of-band observation of the child process.

### 1. Process model: child-per-run

Each isolated run spawns a fresh child process, runs exactly one entry program in
it, and lets the child exit. There is no warm pool and no process reuse in v1.

This is chosen for isolation simplicity, not throughput: a fresh child inherits
no state from a previous run, so a run cannot leak the previous run's heap,
[virtual filesystem](../../CONTEXT.md), realm, or partially-torn-down native
state into the next. A warm/pooled child would have to prove it fully reset all
of that between runs, which is exactly the class of latent-state bug the split is
meant to remove. Pooling is recorded as an out-of-scope follow-up (§7), matching
0106's decision to keep v1 narrow.

The child is the sandbox runner binary itself, re-entered in a child mode over
stdio rather than a second executable. Because parent and child are the same
build, the protocol version matches by construction; the version check (§3) is
defense-in-depth against a mixed-binary deployment, not an expected runtime path.
Phase B owns the exact re-exec mechanism (argv flag or environment marker); this
ADR fixes only that the child speaks this protocol on its standard streams. An
*inherited-descriptor* re-exec mechanism is deliberately excluded: it would add a
fourth inherited descriptor, which the descriptor boundary below forbids.

**Inherited process state is constrained now, even though the mechanism is
deferred.** Whatever re-exec Phase B picks, the inherited-resource boundary is an
isolation property Phase A fixes, not a Phase B liberty — otherwise a child could
follow the wire protocol perfectly while still holding host resources:

- **Descriptors:** the child inherits only the three standard streams the
  protocol uses (stdin, stdout, stderr). No other parent descriptor is passed
  down — not an already-open host file or socket, and not a control descriptor
  for re-exec signalling — so nothing can leak across the boundary and the
  re-exec mechanism (above) may not smuggle a fourth descriptor in.
- **Environment:** the parent's environment is not a channel into the child. The
  child inherits nothing from it beyond what the chosen mechanism strictly needs
  (at most a single re-exec marker), so host secrets in the parent's environment
  are not visible to guest source.
- **Working directory:** the child process still has an operating-system working
  directory — the in-memory VFS is the *guest* file view (§3a), not a replacement
  for the process's OS working directory. Guest file APIs resolve only against the
  VFS and never consult the OS working directory, which is set to a neutral
  location that grants no reach. This is not filesystem confinement: raw
  host-filesystem access from a child that has escaped into native code stays
  outside the Phase A guarantee (§5), exactly as §3a records.

Only the concrete plumbing — which flag, which marker — is Phase B's to choose;
these constraints on what may cross the boundary are not.

### 2. IPC transport: stdio, length-prefixed frames

The parent writes to the child's stdin and reads from the child's stdout. Both
directions carry **length-prefixed frames**. There is no shared memory in v1.

- **stdin (parent → child):** exactly one `run-request` frame, then EOF.
- **stdout (child → parent):** exactly one `run-result` frame, then the child
  exits. stdout carries framed protocol bytes only.
- **stderr (child → parent):** reserved for unstructured child diagnostics —
  crash backtraces, FPC runtime error text, assertion output. It is never part
  of the framed protocol, so a crash dump can never corrupt the result channel.
  The parent may capture stderr and fold it into a crash message (§4) but must
  never parse it as a frame.

**Frame format.**

```text
+-------------------+------------------------------+
| length (4 bytes)  | payload (length bytes)       |
| uint32, big-endian| UTF-8 JSON document          |
+-------------------+------------------------------+
```

- **Length prefix:** a 4-byte unsigned integer, **big-endian** (network byte
  order), giving the payload length in bytes. The prefix counts the payload
  only, not the 4 prefix bytes.
- **Payload:** a single UTF-8-encoded JSON document (§3). Binary blobs inside the
  payload — seed bytes and file contents in the baseline and diff — are
  base64-encoded strings, using the standard alphabet the runner already decodes
  (`TryDecodeBase64Standard`). This reuses the engine's existing JSON parser,
  JSON serializer, and base64 path rather than introducing a second wire codec.
  A binary TLV encoding was considered and rejected for v1: the transport is a
  local pipe for a child-per-run model, so encoding density is not the
  bottleneck, and one encoding is cheaper to keep correct than two.
- **Max frame size:** `MAX_FRAME_BYTES = 64 * 1024 * 1024` (64 MiB). This is
  chosen to comfortably exceed the 16 MiB default filesystem byte quota
  (`DEFAULT_SANDBOX_BYTE_QUOTA`) with headroom for base64 expansion and JSON
  structure, so a fully-materialised baseline or diff always fits in one frame.
- **Oversize handling, child → parent:** the parent reads the 4-byte length
  first. If the declared length exceeds `MAX_FRAME_BYTES`, the parent **does not
  allocate** the buffer; it terminates the child and classifies the run as
  `sfkChildProcessCrash` (§4). An oversize declared length is a protocol
  violation by an untrusted child, treated as death, never trusted enough to
  size an allocation from.
- **Oversize handling, parent → child:** the parent constructs the request frame
  itself, so it must guarantee the frame fits. It measures the UTF-8 byte length
  of the **complete serialised `run-request`** — every variable field, not the
  materialised baseline alone: config, capability grants, import map and aliases,
  and the base64-expanded seed bytes all count — and if that total exceeds
  `MAX_FRAME_BYTES` it **fails before spawning** with `sfkHostError` (a host
  misconfiguration, not source misbehaviour). The filesystem byte quota
  (`--fs-quota-bytes`) large enough to inflate the baseline is the common cause,
  but the check is on the whole frame so no field can silently push the request
  over. With the default configuration this cannot happen.
- **Exact-length pipe I/O:** a successful pipe `read` or `write` may transfer
  fewer bytes than requested — that is normal pipe behaviour, not an error. Both
  sides therefore read and write the 4-byte prefix and the payload in
  **exact-length loops**: continue issuing the operation for the remaining bytes
  until the requested count has been transferred, EOF is reached, or a genuine
  error occurs. `EINTR` (and `EAGAIN` when non-blocking descriptors are used) is
  retried, never surfaced. Only after EOF or a genuine error does the failure
  taxonomy apply (§4): a short read is never itself classified — a prefix or
  payload cut short by EOF is an incomplete frame (`sfkChildProcessCrash` when
  reading from the child; `sfkHostError` when the parent's own write fails).
- **Single-frame, non-streaming:** each direction sends exactly one frame per
  run. There is no framing for incremental or streamed output in v1; streaming
  frames are an out-of-scope follow-up (§7).

### 3. Message schema and versioning

Every frame is a JSON object with two mandatory fields: `type` (a string
discriminator) and `v` (an integer protocol version, `1` for this ADR). The
child must read and validate `v` before acting on any other field.

**Versioning rule — exact match, no negotiation.** The parent stamps `v` into the
`run-request`. If the child's supported version differs from the request's `v`,
the child does **not** run the entry program. It emits a single `run-result` with
`ok: false`, `failureKind: "host-error"`, an `errorMessage` naming both versions,
and exits. The parent independently rejects any `run-result` whose `v` does not
match its own and, on either mismatch, reports `sfkHostError`. A version mismatch
is a deployment fault (mismatched parent/child binaries), not untrusted-source
misbehaviour, so it maps to `sfkHostError`, never `sfkChildProcessCrash`. v1 does
no downgrade or negotiation; negotiation is an out-of-scope follow-up (§7).

#### 3a. `run-request` (parent → child)

```json
{
  "type": "run-request",
  "v": 1,
  "entry": "/main.js",
  "engine": {
    "sourceType": "script",
    "compatibility": [],
    "warnUnsupportedFeatures": false,
    "strictTypes": false,
    "unsafeFunctionConstructor": false,
    "unsafeShadowRealm": false,
    "maxMemoryBytes": 67108864,
    "fetch": {
      "denyPrivateRanges": false,
      "maxResponseBytes": 8388608
    }
  },
  "execution": {
    "mode": "interpreter",
    "timeoutMs": 0,
    "maxInstructions": 0,
    "deterministic": false,
    "unsafeFFI": false,
    "fsQuotaBytes": 16777216,
    "fsNodeLimit": 4096,
    "importMap": null,
    "aliases": []
  },
  "capabilities": {
    "allowedFetchHosts": [],
    "hostFilesystemLoading": false
  },
  "run": {
    "isolated": true,
    "includeDiff": false,
    "diffMetadata": false,
    "diffFormat": "json"
  },
  "baseline": {
    "files": [
      { "path": "/main.js", "kind": "file", "base64": "..." },
      { "path": "/data",    "kind": "dir" }
    ]
  }
}
```

- **`engine`** serialises *exactly* the option set that
  `ApplyFileConfigToEngine` honours, so the child configures an engine identical
  to the in-process one and the protocol cannot drift from the runner's real
  config surface. That set is: `sourceType`; the eleven **compatibility flags**
  (`compat-asi`, `compat-var`, `compat-function`, `compat-traditional-for-loop`,
  `compat-while-loops`, `compat-loose-equality`, `compat-non-strict-mode`,
  `compat-arguments-object`, `compat-label`, `compat-for-in-loop`,
  `experimental-js-module-source`), carried as an array of the enabled flag
  names in `compatibility`; `warnUnsupportedFeatures`; `strictTypes`;
  `unsafeFunctionConstructor`; `unsafeShadowRealm`; `maxMemoryBytes` (the
  resolved `--max-memory`); and the fetch policy pair `denyPrivateRanges` and
  `maxResponseBytes`. These are the flag-resolved *effective* values, already
  merged across CLI, per-file config, and root config on the parent — the child
  receives resolved values and does no config-file resolution of its own.
- **`execution`** carries the sandbox-runner execution controls that are applied
  at scope-push and context-creation time rather than through
  `ApplyFileConfigToEngine`: execution `mode`, the cooperative `timeoutMs`
  (`PushTimeoutScope`), `maxInstructions` (`PushInstructionLimitScope`),
  `deterministic` (selects the deterministic host-environment profile),
  `unsafeFFI`, the filesystem quotas that size the child's virtual filesystem,
  and module-resolution inputs (`importMap`, `aliases`). `timeoutMs: 0` and
  `maxInstructions: 0` mean unbounded, matching `ValueOr(0)` today.
- **`capabilities`** are the grants the child is permitted to exercise:
  `allowedFetchHosts` (the fetch allowlist; empty means fetch is blocked) and
  `hostFilesystemLoading` (false for sandbox runs — the guest is granted no
  host-filesystem module loading). `allowedFetchHosts` here is host
  *authorization*; `engine.fetch`'s `denyPrivateRanges` and `maxResponseBytes`
  are *transport* policy — they are not the same list, so the enforcement order
  is fixed, not incidental: host authorization is checked first, and only a
  request whose target is on `allowedFetchHosts` may then be subjected to the
  transport limits. An empty `allowedFetchHosts` blocks every request outright,
  regardless of the transport settings, and transport limits never substitute for
  the host grant — an implementation may not apply `denyPrivateRanges` or
  `maxResponseBytes` in place of enforcing the allowlist. This authorization is
  re-applied to **every redirect hop's resolved target**, not just the initial
  URL: the `allowedFetchHosts` check and, when `denyPrivateRanges` is enabled,
  the private-range denial MUST be evaluated against each hop's resolved
  destination before that hop is connected, so a `302` to an off-allowlist host
  or a private address (for example a loopback URL that redirects to
  `10.255.255.1`) is refused mid-chain even though the initial URL was
  authorized. This mirrors the in-process client, which already re-runs
  `ResolveAndValidateDestination` per hop rather than trusting the first
  authorization.
- **`baseline`** is the fully **materialised** seed baseline, not the seed specs.
  The parent resolves every seed — including `sskParentPath` seeds that read the
  parent's host filesystem and `sskText`/`sskBytes` inline seeds — into concrete
  virtual-filesystem entries before sending. Each entry is `{ path, kind }` with
  `kind` `"file"` or `"dir"`; files carry `base64` content. This is the load-
  bearing isolation property: the child receives bytes, never paths into the
  parent's host, so a sandbox run needs no host-filesystem access to execute.
  This narrows to what Phase A actually buys — the child is not *given* host
  paths. Stopping a *compromised* child, one that has escaped the engine into
  native code, from reaching the host filesystem through raw syscalls is a
  syscall-level jail, which §5 defers; Phase A/B provide process separation, not
  OS-level filesystem confinement. Materialisation on the parent side also means
  host-path symlink rejection and quota accounting stay where they already are.

#### 3b. `run-result` (child → parent)

```json
{
  "type": "run-result",
  "v": 1,
  "ok": true,
  "exitCode": 0,
  "failureKind": "none",
  "errorMessage": "",
  "output": "…captured stdout console lines…",
  "errorOutput": "",
  "resultValue": null,
  "diffRequested": false,
  "diff": null,
  "truncated": false
}
```

- **`failureKind`** is one of `"none"`, `"script-error"`, `"resource-limit"`,
  `"timeout"`, or `"host-error"` — the five in-band values a child can report
  about itself. A child **never** emits `"child-process-crash"`; that value is
  set only by the parent (§4).
- **`resultValue`** is the cloned script result, serialised with the same
  structural clone the runner already performs (`CloneResultValue`) reduced to
  its JSON-expressible shape — `null`, boolean, number, string, array, object —
  with cycles broken as they are today. Non-clonable values serialise as `null`,
  matching the in-process clone.
- **`diff`** is a string when `diffRequested` is true: a JSON diff document when
  `diffFormat` is `"json"`, or unified-diff text when `"unified"`. The child
  computes the diff itself, exactly as `ExecuteSandboxPath` does today — it
  captures its post-seed baseline, runs, and diffs — so the parent relays a diff
  rather than reconstructing the child's virtual filesystem. `diff` is `null`
  when no diff was requested, and also `null` in the overflow fallback below
  even when `diffRequested` is true; the `truncated` flag is what distinguishes
  "diff omitted for size" from "no diff requested", so a `null` `diff` is never
  ambiguous.
- **`output`** and **`errorOutput`** are the captured console and error streams.
  The in-process runner does not cap capture today (`FCurrentOutputLines` is an
  unbounded list), so bounding it is a protocol requirement here, not a runner
  nicety: a run that prints more than a frame can hold would otherwise emit an
  oversize `run-result` and be misread as a crash by the parent's §2 oversize
  handling, even though it ran cleanly. The child therefore truncates each stream
  to a documented capture budget sized so that the stream text, plus the
  materialised `diff` and the rest of the result object, stays within
  `MAX_FRAME_BYTES` (§2); truncation appends an explicit marker so the loss is
  visible rather than silent. The exact budget is Phase B's to fix, but bounding
  capture on the child side — never relying on the parent's oversize path to
  absorb legitimate output — is normative.
- **`truncated`** is a boolean, `false` on every frame except the overflow
  fallback below, where it is `true`. It is the single flag the parent reads to
  learn that a **clean** run's frame was reduced to fit `MAX_FRAME_BYTES` — that
  `diff` and `resultValue` were dropped to `null` and `errorMessage` was cut —
  rather than that the run produced no diff or no result. The parent treats a
  `truncated` frame as a successful, lossy run: it keeps `ok`, `exitCode`, and
  `failureKind` verbatim and surfaces the truncation to the host, and it **never**
  reclassifies a `truncated` frame as `child-process-crash` (§2/§4), because the
  frame is well-formed and complete — it is the payload, not the framing, that was
  bounded.
- **Total frame size** is bounded, not only the two captured streams. A
  per-stream capture budget alone does not guarantee frame fit: `diff`,
  `resultValue`, and `errorMessage` are also variable-length and must each carry a
  documented UTF-8 byte limit. Every truncation — `output`, `errorOutput`, and
  `errorMessage` — cuts at a **code-point boundary**, never mid-sequence, so the
  result is always well-formed UTF-8, and it reserves the marker's byte length
  before cutting so the marker plus the retained text still fit the field's limit.
  After serialisation the child re-checks the whole `run-result` against
  `MAX_FRAME_BYTES` as the final guard; if it still overflows (a legitimately
  large clone or diff), the child applies a **deterministic** fallback rather than
  emitting an oversize frame the parent would misread as a crash — it drops
  `diff` and `resultValue` to `null`, truncates `errorMessage` to its byte limit
  at a code-point boundary, and sets the `truncated` field (defined above) to
  `true`, in that fixed order,
  so a clean run always yields a schema-valid, well-formed frame. The `truncated`
  field is what keeps this fallback consistent with the schema: `diff` may be
  `null` with `diffRequested` still `true`, and the parent disambiguates that from
  a genuinely empty result solely by `truncated`. Guaranteeing frame fit for every
  clean run is normative; the exact per-field limits are Phase B's to fix.

### 4. Failure taxonomy mapping

The classification has two axes: what the **child reports in-band** in a
well-formed `run-result`, and what the **parent sets out-of-band** when it
observes the child process. The two never overlap.

**In-band (child ran and produced a `run-result` frame).** The child maps its
own outcome onto `failureKind`:

| Child-side condition | `failureKind` |
| --- | --- |
| Clean completion | `none` |
| Parse or link failure, a missing entry path, or an uncaught script error (`TGocciaError` — including `TGocciaSyntaxError` — or `TGocciaThrowValue`) | `script-error` |
| Instruction, memory, or filesystem-quota ceiling exceeded (`TGocciaInstructionLimitError`, `TGocciaMemoryLimitError`, `ESandboxFsQuotaExceeded`) | `resource-limit` |
| Cooperative timeout fired *inside* the child, cleanly | `timeout` |
| Capability-audit delivery or other host/Pascal fault | `host-error` |

The child reporting `timeout` is the **cooperative** case: a poll point observed
the deadline, unwound cleanly, and the child still emitted a trustworthy result
frame. This mirrors the in-process failure branches in
`ExecuteSandboxPathInContext` — a memory-limit error is reported as the limit it
is, a script throw as a script error — now surfaced as an enum instead of only a
message string.

**Out-of-band (parent observed the child process, no trustworthy frame).** The
parent sets `sfkChildProcessCrash` — the reserved value — when a child that
**did start** then fails to deliver a well-formed in-band `run-result`:

- the child exits without ever writing a complete result frame (EOF mid-frame,
  short read, truncated payload, non-JSON payload, or an oversize declared
  length per §2);
- the child is terminated by a signal it did not itself request (`SIGSEGV`,
  `SIGABRT`, `SIGBUS`, an OOM-killer `SIGKILL`); or
- the parent's deadline elapses and the parent **hard-kills** the child because
  the child never reached a poll point to report a cooperative timeout.

A failure on the parent's **own** side is *not* a child crash and never takes the
reserved value: the child never spawns (`fork`/`exec`, `posix_spawn`, or
`CreateProcess` fails), or a parent-side pipe, write, read, or wait syscall
fails. No child outcome was ever observed in these cases, so they are
`sfkHostError` — the same class as a version mismatch (§3). `sfkChildProcessCrash`
is reserved for a child that *started* and then died or produced an untrustworthy
frame; it never absorbs a parent-side transport or spawn fault. An *interruptible*
wait failure is not yet a wait failure: a `wait`/`waitpid` that returns `EINTR`
(or `EAGAIN` on a non-blocking wait) is retried, and the outcome is classified
`sfkHostError` only after a retried wait establishes a genuine parent-side fault.

This is the crisp crash-vs-in-band distinction: **"child died"** — the parent
observed the death and has no trusted frame — is always `sfkChildProcessCrash`,
while **"child ran and reported a limit"** — a complete frame carrying
`resource-limit` or `timeout` — is always the in-band value. `sfkChildProcessCrash`
means *the parent could not trust an in-band result*, nothing narrower.

**The parent deadline, normatively.** `execution.timeoutMs` is the child's
*cooperative* budget. The parent's hard deadline is derived from it —
`timeoutMs` plus a fixed grace margin — so the child always gets first chance to
observe the deadline at a poll point and emit a clean `timeout` frame; only after
the grace elapses does the parent hard-kill. `timeoutMs: 0` (unbounded) means the
parent arms no deadline. Precedence when a result races the deadline is settled
by the same evidence rule, not by timing luck: if the parent has read a complete,
trustworthy `run-result` before it hard-kills, that in-band frame wins (and a
`timeout` frame reads as `timeout`); if the parent hard-kills first and no such
frame was read, it is `sfkChildProcessCrash`. The grace margin's exact value is
Phase B's to fix; that the parent deadline is `timeoutMs`-derived, grace-delayed,
and evidence-resolved is not. The evidence rule outranks the reaping outcome as
well: once the parent has read a complete, trustworthy `run-result`, that frame is
authoritative even if the subsequent `wait` fails — a reap error after a trusted
frame is logged, never a reclassification, so a valid result is never downgraded
to `sfkHostError` by a wait that failed only after the outcome was already known.

The parent's synthesised crash result preserves the cause in `errorMessage`: a
deadline hard-kill records `"execution deadline exceeded; child hard-killed after
Nms"`, an unexpected signal records the signal number and any captured stderr
tail. That stderr tail is **byte-capped** before it is folded into `errorMessage`:
the parent retains only a bounded UTF-8 tail (the exact bound is Phase B's to
fix), so a child that floods stderr before dying cannot drive unbounded
parent-side diagnostic memory or push `errorMessage` past its §3b byte limit. The
signal is carried **in that message**, not in a separate field — the
`run-result` schema (§3b) and `TGocciaSandboxRunResult` define no `signal`
member, by the same "cause lives in `errorMessage`" choice this section makes
throughout. A host that needs to tell "timed out, killed" apart from "crashed on
`SIGSEGV`" reads that message, not a separate enum value or signal field.

> **Decision 0106 did not settle — flagged for review.** 0106 says Phase B adds
> "a parent-enforced deadline and hard kill" but does not say which `failureKind`
> a deadline **kill** produces. This ADR classifies a parent deadline-kill as
> `sfkChildProcessCrash` (the parent observed a death with no trusted frame), and
> reserves `sfkTimeout` for the child's own **in-band, cooperative** timeout. The
> alternative — classifying a deadline-kill as `sfkTimeout` because the parent
> knows the cause — was rejected to keep the crash boundary defined purely by
> "was there a trustworthy in-band frame," so `sfkTimeout` never has two
> provenances. Cause is preserved in `errorMessage` either way. If reviewers
> prefer deadline-kills to read as `sfkTimeout`, that is a one-line change to the
> parent's synthesised classification and does not touch the wire format.

### 5. Platform jail matrix (deferred, not implemented)

Per 0106 the per-platform syscall jails are **deferred**, with the matrix
recorded. None of the following ships in Phase A or Phase B. They describe a
hypothetical Phase C and are recorded so that phase does not restart from a blank
page. Process separation (Phase B) is platform-uniform; a jail would add
OS-specific confinement *inside* the already-separated child.

| Platform | Mechanism (Phase C, hypothetical) | What it would restrict | Phase C posture |
| --- | --- | --- | --- |
| Linux | seccomp-bpf + `no_new_privs` + `setrlimit` (`RLIMIT_AS`/`RLIMIT_CPU`/`RLIMIT_NOFILE`/`RLIMIT_NPROC`) | Confine the child to a syscall allowlist, forbid privilege gain via exec, and cap address space, CPU, descriptors, and processes | Out-of-process only |
| OpenBSD / FreeBSD | `pledge` + `unveil` (OpenBSD); Capsicum capability mode (FreeBSD) | Reduce the child to a `stdio`/`inet` promise set and hide the host filesystem namespace it never needs (the VFS is in-memory) | Out-of-process only |
| Windows | Restricted / lowbox token + job object (`JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE`, active-process and memory caps) | Strip token privileges, cap memory and process count, and kill the child tree when the parent exits | Out-of-process only |
| macOS | `sandbox-exec` seatbelt profile, or `posix_spawn` confinement attributes + `setrlimit` | Restrict file and network reach via a seatbelt profile and cap memory/CPU via rlimits | **Ship jailed (candidate)** |

**Ship-jailed vs out-of-process split.** In a future Phase C, only the
author-executable platform — **macOS**, per 0106's note that three of the four
implementations "cannot be executed by their author" — is a candidate to ship
*jailed*. **Linux, the BSDs, and Windows** would ship **out-of-process only**
until each jail is validated by someone who can actually run and test it on that
platform. This is the same reasoning 0106 used to defer the jails wholesale:
shipping an unrunnable-by-its-author confinement is worse than shipping none,
because a jail that is never exercised gives false assurance. A deploying host
that wants confinement today obtains it by running the out-of-process child
inside a container or VM, which 0106 already records as the available answer.
This split is provisional and gated on execution access, not decided now.

### 6. Fetch reconciliation

Fetch is **not brokered** in this design, consistent with 0106. When a run is
isolated, the engine still runs in the child, so `fetch` continues through the
existing in-process fetch path *in the child* — the same allowlist recheck,
one-time DNS resolution, resolved-address validation, and per-redirect-hop
address pinning that [ADR 0103](0103-layered-untrusted-execution-boundaries.md)
and 0106 already define. The child receives the allowlist and fetch policy in the
`run-request` (`capabilities.allowedFetchHosts`, `engine.fetch`) and enforces
them itself; no socket is passed across the process boundary and the parent runs
no proxy. A broker would only earn its serialization surface and its second copy
of the address policy if a Phase C network jail blocked the child's own sockets —
and 0106 committed to no such jail. With the child keeping its own network
access, brokering would add cost and a divergence risk for nothing gained.

### 7. Out of scope, recorded as follow-ups

These are deliberately excluded from v1 and left as named follow-ups:

- **Warm / pooled children.** Reusing a child across runs, to amortise spawn
  cost, requires proving full inter-run state reset (heap, VFS, realm, native
  state) — the exact latent-state risk child-per-run removes. Deferred until
  spawn cost is shown to matter.
- **Windows AppContainer.** A stronger Windows confinement than the restricted
  token / job object in §5; deferred with the rest of the Windows jail and
  separately called out because it is a distinct, heavier mechanism.
- **Embedding-API (non-CLI) isolation.** This protocol isolates the CLI sandbox
  runner. Isolating an in-process embedding host that links the engine as a
  library is a different boundary (no stdio child to speak to) and is not
  addressed here.
- **Protocol version negotiation / downgrade.** v1 is exact-match only (§3).
- **Shared-memory and streaming transports.** v1 is one stdio frame per direction
  (§2); zero-copy shared memory and incremental/streamed frames are future work.

## Consequences

The wire format is fixed for a Phase B implementer: 4-byte big-endian length
prefix, 64 MiB max frame, one UTF-8 JSON `run-request` in and one `run-result`
out over stdio, base64 for binary, stderr reserved for unstructured diagnostics.
No part of the frame or schema is left to Phase B's discretion.

The engine configuration carried on the wire is pinned to the
`ApplyFileConfigToEngine` set plus the sandbox-runner execution controls, so the
isolated child and the in-process runner configure identical engines. If the
runner's honoured option set changes later, this schema changes with it in
lockstep — the coupling is intentional, so the protocol cannot silently honour a
different set than the runner does.

The classification boundary is defined by evidence, not by cause: any run whose
child died without a trustworthy in-band frame is `sfkChildProcessCrash`, and only
a complete frame carries the in-band kinds. This makes the reserved value's
meaning precise and keeps `sfkTimeout` single-provenance, at the cost that a
deadline hard-kill reads as a crash rather than a timeout unless the caller
inspects `errorMessage` — a trade this ADR flags for review rather than deciding
silently.

Fetch behaviour is unchanged by isolation: an isolated run reaches the network
exactly as an in-process run does, governed by the same address policy, because
there is no jail for a broker to complement. This keeps one implementation of the
fetch policy and defers any brokering to a Phase C that 0106 did not commit to.
