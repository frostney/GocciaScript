# 0124 - An unhandled promise rejection fails the run

**Date:** 2026-10-01
**Area:** `runtime`, `cli`, `testing`

## Context

A promise that was rejected and never got a handler was silently dropped.
`Promise.reject(new Error("x"));` and an un-awaited `async` call that threw
printed nothing and `GocciaRunner` exited 0, so a forgotten `await` looked like
success ([#1295](https://github.com/frostney/GocciaScript/issues/1295)). An
awaited rejection at the top level was already reported as an uncaught error.

ES2026 §27.2.1.9 leaves this to the host: `HostPromiseRejectionTracker` is
called with "reject" when a promise is rejected with no handler and with
"handle" when a handler is added to such a promise later. Node.js and Deno
report the rejection and exit non-zero by default.

## Decision

**The engine tracks rejections as the spec describes.** A promise carries
`[[PromiseIsHandled]]`. Rejecting an unhandled promise registers it with the
microtask queue; attaching a handler, awaiting it, or a host path that rethrows
its reason removes it. The registration lives in the promise's microtask scope
([ADR 0123](0123-per-execution-microtask-scopes.md)), so a nested engine's
rejection is its own and its caller's is not blamed on it.

**What is still registered when a run has nothing left to do fails the run.**
`Execute`, `ExecuteProgram`, `RunModule` and `RunModuleInScope` raise the
reason of the first such promise like an uncaught throw. Every existing
consumer of an uncaught throw therefore reports it without new code: the CLI's
code frame and exit status, the JSON envelope's `error`, and a `runScript`
child's `failureKind`.

**Embedders get a setting, and the default is to fail.**
`Engine.UnhandledRejections` is `urThrow` by default. A silent default is the
bug being fixed, so a new host has to opt out of failing rather than remember
to opt in. `urIgnore` turns the raise off and leaves the promises registered.
A host can take the oldest with `TakeUnhandledRejection`, which forgets the
others, while the run is in progress or after an `ExecuteProgram` or
`RunModule` that returned normally; `Execute` clears its queue before it
returns, so after it there is nothing left to take.

**A registration never outlives what could report it.** A registered promise
is kept alive, so one that nothing will ever take is a leak into the next
engine. A run that fails takes its registrations with it; an engine that is
destroyed while no run is in progress drops what is registered on the thread,
which is what code called after its run left; the benchmark runner drops them
after each batch, because a benchmark measures and does not assert; and the
sandbox host reads a child's result inside a scope of its own, so a getter
that rejects ends with the child.

**The command line has no opt-out.** Nothing in the repository needs one.

**Hosts that run many units in one run attribute the rejection themselves.**
The test runner takes what is registered after each test, each hook and
collection, once that unit's queued jobs have run, and fails that unit. What
the file's top level left fails the file before collection, in both execution
modes. It does this under the default setting: taking a promise is what keeps
the engine from raising it.

**The test262 runner ignores them.** Conformance tests leave promises rejected
on purpose, and the harness reports asynchronous failures through `$DONE`.

**The REPL reports and continues.**

## Considered options

- **A callback hook (`OnUnhandledRejection`).** More flexible than a setting,
  and the only way for a host to see the rejections of a run made with
  `Execute` after it returns. Rejected for now: no host in the repository
  needs that, and `TakeUnhandledRejection` covers hosts that attribute
  rejections during a run, as the test runner does.
- **Ignore by default, with the CLI opting in.** No change for existing
  embedders. Rejected: every new host would swallow rejections silently.
- **A `--unhandled-rejections` flag.** Rejected until a script needs it.

## Consequences

- A script that left a rejection unhandled and exited 0 now exits 1, and an
  embedder's `Execute` can raise where it returned. This is a breaking change.
- Measured before the change: 3 of 12,914 tests in the suite left a rejection
  on purpose and now give it a handler; the CLI tests and examples left none;
  `benchmarks/promises.js` leaves one per iteration, which the benchmark
  runner drops; 49 test262 tests leave one by design, which is why that
  runner opts out.
- Interpreted `import()` used to fulfil before the imported module's
  top-level `await` had settled, so a script could not catch its rejection.
  It now settles from the module's evaluation promise, as bytecode mode does.
- A handler attached only after the run has finished is too late, as it is in
  Node.js.

## Related

- [Errors — Unhandled promise rejections](../errors.md#unhandled-promise-rejections)
- [Testing API — Async Tests](../testing-api.md#async-tests-promises)
