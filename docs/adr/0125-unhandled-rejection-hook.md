# 0125 - A host can observe unhandled rejections through an engine hook

**Date:** 2026-10-02
**Area:** `runtime`, `embedding`

## Context

[ADR 0124](0124-unhandled-rejections-fail-the-run.md) made a promise that is
still rejected with no handler fail the run, and gave embedders a setting,
`Engine.UnhandledRejections`, to turn that off. It considered a callback hook
and rejected it for the time being, because no host in the repository needed
one.

That left a gap ([#1334](https://github.com/frostney/GocciaScript/issues/1334)).
Under `urIgnore` a host can only ask the microtask queue with
`TakeUnhandledRejection`, which hands over the oldest rejection and forgets
the others, and `Execute` clears its queue before it returns. A host that runs
scripts with `Execute` and wants to log or count what they left rejected,
without failing the run, could not see anything.

## Decision

**`TGocciaEngine.OnUnhandledRejection` observes; `UnhandledRejections` still
decides.** Once a run has nothing left to do, the engine calls the hook for
each promise the run left rejected with no handler, oldest first, with the
promise and its reason. Then the mode is applied as before: `urThrow` raises
the oldest rejection's reason, `urIgnore` raises nothing. With the hook unset
nothing changes.

**The hook is called where the run would fail**, after the idle drain and
before `Execute` clears its queue, from `Execute`, `ExecuteProgram`,
`RunModule` and `RunModuleInScope`, in both execution modes. A nested engine
reports its own rejections to its own hook.

**A promise the hook gives a handler is handled.** The hook runs before the
mode is applied, so a host that deals with a rejection there keeps the run
from failing for it, and a promise handled while an earlier one was being
reported is not reported.

**Under `urIgnore` a reported promise is forgotten.** `ExecuteProgram` and
`RunModule` clear nothing, so a promise left tracked would be reported again
at the engine's next idle point. `TakeUnhandledRejection` therefore finds
nothing after a run that reported through the hook.

**A run that ends by exception reports nothing.** It has reported itself by
failing, and what it left rejected goes with it, as ADR 0124 decided.

## Considered options

- **A hook that returns whether the run should fail.** One mechanism instead
  of a hook and a setting. Rejected: the setting exists and is the default
  path, and a host that wants to decide per rejection can already do it by
  giving the promise a handler in the hook.
- **Keep reported promises tracked under `urIgnore`.** Leaves
  `TakeUnhandledRejection` usable after the run. Rejected: the same promise
  would be reported once per run on engines driven with `ExecuteProgram`.
- **Report rejections of a run that failed.** Rejected: the run's error is the
  report, and a host would see rejections that the failure itself caused.

## Consequences

- The hook may run script, for example by reading the reason's `message`. A
  rejection that script leaves is not reported; under `urThrow` it can still
  fail the run if it is the oldest one left.
- An exception the hook raises ends the run like any other host error.
- The CLI tools do not install a hook; their behavior is unchanged.

## Related

- [ADR 0124 — An unhandled promise rejection fails the run](0124-unhandled-rejections-fail-the-run.md)
- [Embedding — Error Handling](../embedding.md#error-handling)
