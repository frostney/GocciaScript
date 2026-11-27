# Every application stops on an engine fault

**Date:** 2026-11-27
**Area:** `runtime`, `errors`, `cli`
**Related:** [ADR 0109](0109-engine-integrity-faults-are-uncatchable.md), [ADR 0188](0188-the-delphi-projects-and-the-wasm-test-runner-leave-with-the-old-engine.md)

## Context

[ADR 0109](0109-engine-integrity-faults-are-uncatchable.md) made engine-integrity
faults uncatchable in two tiers: every script boundary re-raised them, and
`GocciaTestRunner` aborted the run with exit `70` when one reached it. It left
one boundary open on purpose: the generic handler of `TGocciaApplication.Run`,
which every application runs under, still reported a fault as an ordinary error
with exit `1`.

Both tiers belonged to the old engine and went with it when every application
moved to the new one. The new engine's conversion boundaries (sandbox
filesystem calls, FFI, data-format parsers, snapshot matchers, module
resolution, the compiler entry points) were written without the guard, and the
new applications' handlers had none either. On 0.18.0 an access violation in
guest code made `GocciaRunner` print `Error: Access violation` and exit `1`
([#1167](https://github.com/frostney/GocciaScript/issues/1167)), and made
`GocciaTestRunner` record the fault as one failed test, run the remaining files,
print its summary and exit `1`
([#1166](https://github.com/frostney/GocciaScript/issues/1166)). The ADR's own
coverage section explains why nothing caught it: the abort had no automated
test, because reaching it needed a fault on request in the runner.

## Decision

**Every application stops on an engine fault and exits `70`.** That covers
`GocciaRunner` (host and sandbox mode), `GocciaTestRunner`,
`GocciaBenchmarkRunner`, `GocciaBundler`, `GocciaREPL`,
`GocciaScriptLoaderBare`, and the coordinators of `GocciaTest262Runner` and
`GocciaTOMLComplianceRunner`, from the application's construction to its
destruction and in each worker thread up to its last cleanup. The handler that
catches the fault calls `Goccia.EngineFault.AbortOnEngineFault`, from whichever
thread it runs on. It writes two lines to stderr, flushed:

```text
Engine fault: EAccessViolation in tests/example.test.js: Access violation
Engine fault: the run was stopped because the engine can no longer vouch for its own state; no result of this run can be trusted.
```

and ends the process with `_exit` (`TerminateProcess` on Windows). It does not
unwind: an unwind would run the handlers between there and the end of the
program on the state the fault calls into question, and let the application go
on to print the summary or JSON report it would print for a run that finished.
No other file is started and nothing more is reported. The mechanics are ADR
0109's, moved from the test runner into the shared unit: a first-fault gate so
only one thread writes, and a bounded wait so a second faulting thread cannot
end the process in the middle of that write. Three things are new. Before
writing, the faulting thread flushes its own buffered stdout, so what the
script printed before the fault comes out ahead of the diagnostic; output other
threads still buffer dies with the process. A write to a closed stderr pipe
fails instead of raising `SIGPIPE`, and a watchdog thread ends the process with
`70` if the write blocks on a full pipe, so a broken stderr changes neither the
exit status nor whether the process ends. And the engine's own integrity checks
(a heap cell, a shape, a closure or a root count found broken, often in the
middle of a collection) write what they found and then call
`StopOnEngineCheck`, which ends the process the same way without raising,
instead of `Halt(9)`, which ran unit finalization from the thread that found
it.

The prefix is `Engine fault:` rather than ADR 0109's `Integrity fault:`,
because the family below now includes assertions and stack overflows, and every
application prints it.

**The family.** One predicate, `IsEngineFault`, serves script boundaries and
hosts alike. It keeps ADR 0109's classes and adds `EAssertionFailed` (an
internal `Assert`), `EAbstractError` (a call that reached an abstract method)
and `EStackOverflow`. ADR 0109 kept `EStackOverflow` convertible because the old
engine turned it into the guest's "Maximum call stack size exceeded". The new
engine bounds every recursion a script can drive (call frames, the parser's
nesting, JSON, RegExp) and throws that `RangeError` itself, so an
`EStackOverflow` means native code outran the engine's bound. Anything else a
handler catches (a script error, a missing file, an invalid option, a usage
error, a refused allocation) keeps its own report and exit code.

**An engine check raises; it never halts.** `RunError` is a program's own
exit request: it runs `Halt`, with unit finalization, on the calling thread,
and raises nothing a host could report. The engine's check that a native popped
more roots than it pushed used it, so a broken root count ended the process
with exit status `204` from whichever thread found it. Such a check now calls
`Goccia.EngineFault.RaiseRuntimeError`, the path the RTL's own internal errors
take (`FPC_HANDLEERROR`), which raises the mapped exception (`EInvalidPointer`
for `204`) for the handlers above to stop on. A check inside a collection, where
raising would unwind through the collector, stops with `StopOnEngineCheck`
instead (above).

**Where the guard goes.** Every catch-all handler in `source/engine`,
`source/units` and `source/app` that turns an exception into something other
than an exception (a guest value, a result field, an error message, a skipped
item) opens with the guard. At a script boundary it re-raises, as in ADR 0109.
On a thread whose exception would otherwise end the thread silently (a
`$262.agent` thread, a fetch worker, a compliance worker) or where an exception
must not unwind (an FFI callback entered from native code), it calls
`AbortOnEngineFault` instead. `scripts/check-fault-guards.ts` fails CI on a
catch-all handler that neither re-raises, tests for a fault, nor carries a
`fault-guard:` comment saying why it does neither; its `--self-test` keeps the
check itself from passing handlers it should flag.

**Three applications keep a contract of their own**, each stated at the
handler:

- `GocciaFuzzHarness` exits `0` for every outcome the engine models, script
  errors included, and `1` with a backtrace for anything that escapes the
  engine; AFL counts exit `1` as a crash (`AFL_CRASH_EXITCODE=1` in
  `scripts/afl/fuzz.sh`).
- `GocciaTest262Runner` grades a fault inside a case as that case's `crash`
  outcome, naming the exception's class ([test262](../test262.md)). On POSIX
  each case runs in a forked worker process, so the fault cannot reach the
  coordinator. A fault in the coordinator itself exits `70`.
- `GocciaTOMLComplianceRunner` runs each case in a worker process, whose exit
  status `2` the coordinator records as an infrastructure result.

**The fault build.** The contract is tested with real faults: a build with
`-dGOCCIA_FAULT_INJECTION` (`GOCCIA_VARIANT=FaultInjection`) has a
`__gocciaInjectFault(kind)` global in every realm and a `GOCCIA_FAULT_INJECTION`
variable that faults at application startup, as it ends, as a worker thread
releases its engine state, when the bundler compiles an entry, or inside the
engine's handler that makes a failure to compile an entry its `SyntaxError`
(`Goccia.FaultInjection`). `scripts/test-engine-faults.ts` runs every
engine-running application against it on every CI platform and checks the exit
code, both diagnostic lines and the absence of any summary or report, and, on
POSIX, that a closed or full stderr pipe still ends the process with `70`. It
also checks that the shipped binaries carry neither name.
`GocciaTOMLComplianceRunner` runs no engine and is not a build.pas engine
program, which the variant mechanism builds, so its coordinator's handler is
the shared one without a fault test of its own. ADR 0109 decided against a fault hook
in the runner because it would ship in the production binary. A hook that only
a build no shipped target makes can contain does not ship.

## Consequences

A harness reads exit `70` from any application the same way: the process
stopped on an engine fault, and none of its output is a verdict.
`scripts/run_test262_suite.ts` already treats a loader exit above `1` as
wrapper infrastructure, which CI gates at zero.

What this does not reach:

- **A native stack overflow on POSIX.** Code that exhausts its thread's stack
  dies by `SIGSEGV` before any handler runs, so the process ends by the signal,
  not with `70`. FPC 3.3.1 on x86-64 Linux has no stack check that raises
  `EStackOverflow`; a test program ended by `SIGSEGV` under `$S+` as well. The
  engine's own native stack guard keeps script recursion from getting that far.
- **`source/shared`.** Its libraries (`HTTPClient`, `FileUtils`) return
  failures as values by design and know nothing of the engine, so a fault in
  their own code surfaces as the failure they return.
- **`GocciaTest262Runner` without fork.** On Windows the cases run in the
  coordinator's process, and the run goes on after a `crash` outcome.
- **The unwind before the handler.** The `finally` blocks between the faulting
  instruction and the handler that catches it still run, as they did under ADR
  0109: the abort happens at the first handler, not at the fault.
