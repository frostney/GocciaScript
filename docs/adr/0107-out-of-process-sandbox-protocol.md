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
Phase B owns the exact re-exec mechanism (argv flag, environment marker, or
inherited descriptor); this ADR fixes only that the child speaks this protocol on
its standard streams.

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

```
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
  itself, so it must guarantee the frame fits. If a host configures a filesystem
  byte quota (`--fs-quota-bytes`) large enough that the materialised baseline
  would exceed `MAX_FRAME_BYTES`, the parent **fails before spawning** with
  `sfkHostError` (a host misconfiguration, not source misbehaviour). With the
  default quota this cannot happen.
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
  `hostFilesystemLoading` (false for sandbox runs — the child never reaches the
  host filesystem). Fetch host policy appears here as the capability grant and
  in `engine.fetch` as the transport policy; they are the same allowlist viewed
  as grant vs. mechanism.
- **`baseline`** is the fully **materialised** seed baseline, not the seed specs.
  The parent resolves every seed — including `sskParentPath` seeds that read the
  parent's host filesystem and `sskText`/`sskBytes` inline seeds — into concrete
  virtual-filesystem entries before sending. Each entry is `{ path, kind }` with
  `kind` `"file"` or `"dir"`; files carry `base64` content. This is the load-
  bearing isolation property: the child receives bytes, never paths into the
  parent's host, so a child cannot reach the host filesystem even if it wanted
  to. Materialisation on the parent side also means host-path symlink rejection
  and quota accounting stay where they already are.

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
  "diff": null
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
  when no diff was requested.
- **`output`** and **`errorOutput`** are the captured console and error streams,
  bounded by the run's own limits; both fit within one frame under the frame-size
  argument in §2.

### 4. Failure taxonomy mapping

The classification has two axes: what the **child reports in-band** in a
well-formed `run-result`, and what the **parent sets out-of-band** when it
observes the child process. The two never overlap.

**In-band (child ran and produced a `run-result` frame).** The child maps its
own outcome onto `failureKind`:

| Child-side condition | `failureKind` |
| --- | --- |
| Clean completion | `none` |
| Uncaught script error (`TGocciaError`, `TGocciaThrowValue`) | `script-error` |
| Memory or filesystem quota exceeded (`TGocciaMemoryLimitError`, `ESandboxFsQuotaExceeded`) | `resource-limit` |
| Cooperative timeout fired *inside* the child, cleanly | `timeout` |
| Capability-audit delivery or other host/Pascal fault | `host-error` |

The child reporting `timeout` is the **cooperative** case: a poll point observed
the deadline, unwound cleanly, and the child still emitted a trustworthy result
frame. This mirrors the in-process failure branches in
`ExecuteSandboxPathInContext` — a memory-limit error is reported as the limit it
is, a script throw as a script error — now surfaced as an enum instead of only a
message string.

**Out-of-band (parent observed the child process, no trustworthy frame).** The
parent sets `sfkChildProcessCrash` — the reserved value — whenever it cannot
obtain a well-formed in-band `run-result`:

- the child exits without ever writing a complete result frame (EOF mid-frame,
  short read, truncated payload, non-JSON payload, or an oversize declared
  length per §2);
- the child is terminated by a signal it did not itself request (`SIGSEGV`,
  `SIGABRT`, `SIGBUS`, an OOM-killer `SIGKILL`); or
- the parent's deadline elapses and the parent **hard-kills** the child because
  the child never reached a poll point to report a cooperative timeout.

This is the crisp crash-vs-in-band distinction: **"child died"** — the parent
observed the death and has no trusted frame — is always `sfkChildProcessCrash`,
while **"child ran and reported a limit"** — a complete frame carrying
`resource-limit` or `timeout` — is always the in-band value. `sfkChildProcessCrash`
means *the parent could not trust an in-band result*, nothing narrower.

The parent's synthesised crash result preserves the cause in `errorMessage`: a
deadline hard-kill records `"execution deadline exceeded; child hard-killed after
Nms"`, an unexpected signal records the signal number and any captured stderr
tail. A host that needs to tell "timed out, killed" apart from "crashed on
SIGSEGV" reads that message and the signal, not a separate enum value.

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
