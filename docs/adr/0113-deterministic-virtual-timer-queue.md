# Deterministic virtual timer queue

**Date:** 2026-08-22
**Area:** `runtime`

GocciaScript implements `setTimeout`, `clearTimeout`, `setInterval` and
`clearInterval` over a virtual timer queue in the engine, and builds the Vitest
fake-timer family — `vi.useFakeTimers` and the rest — on top of it in the
test-runner profile. No timer ever waits on wall time: the queue holds a due
time on a virtual clock, and a timer fires only when something advances that
clock.

This supersedes the scope cut in
[ADR 0112](0112-native-async-local-storage.md), which recorded that
"GocciaScript has no timer task queue and no general event loop, so there is no
`setTimeout` continuation for a context to travel into", and predicted that a
host-scheduled callback surface would become a third async-context seam without
changing the snapshot representation. That is exactly what happened: timer
registration captures the current snapshot and the queue installs it around the
callback, through the existing `EnterAsyncContext` / `LeaveAsyncContext` pair.
Nothing in `Goccia.AsyncContext` changed.

## Why this is the engine's problem

The forcing evidence is a corpus sweep of 82 real `convex-test` application
suites. Fake timers were the single dominant engine blocker: 418 of 494
remaining failures, spread across 23 files. `convex-test`'s scheduler is built
on `setTimeout(fn, 0)`, and the suites written against it drain that scheduler
by calling `vi.advanceTimersByTime` / `vi.runAllTimers`. Every one of those
files failed on the first `vi.useFakeTimers()`.

The one corpus file re-run after this landed — `convex/askFeedback.test.ts`,
whose only blocker was `vi.useFakeTimers` — went from 0 of 10 passing to 9 of
10. The one remaining failure is unrelated (`crypto.subtle.digest` is not
provided).

A userland shim cannot supply this. Fake timers are not a wrapper around a real
timer API — they *are* the timer API for the duration of the test, and the code
under test calls the global `setTimeout` directly. Without a `setTimeout` in the
engine there is nothing for a shim to intercept, and with one there is no way
for JavaScript to make the engine's own `await` and end-of-run draining aware of
it.

Declining was the alternative, and it was rejected for the same reason ADR 0112
rejected declining `AsyncLocalStorage`: the gap is not something a suite author
can work around.

## What is virtual, and what real timers mean here

There is one queue and one virtual clock, in two modes.

Under **fake timers** the clock moves only when the suite moves it. That is the
Vitest contract, and it is why `await` deliberately does *not* run timers in
this mode: a suite that turned the clock over to `vi` decided when its timers
run, and an engine that quietly advanced it would take that decision back.

Under **real timers** the clock still never tracks wall time. It jumps forward
to the next timer's due time at the points where the engine would otherwise have
nothing left to do:

- **an `await` on a promise a timer will settle.** GocciaScript drains awaits
  synchronously rather than parking, so `await new Promise(r => setTimeout(r,
  10))` had nowhere to get its continuation from. The queue is consulted in the
  two places that drive a promise to settlement: `AwaitValue`
  (`Goccia.Values.Await`) for a synchronous await, and `WaitForFetchPromise`
  (`Goccia.FetchManager`), which despite its name is the host's general
  settle-this-promise wait and is what the test runner calls on every async
  test's returned promise.
- **the end of each test**, through the runner's own per-test lifecycle
  (`Goccia.Builtins.TestingLibrary`). This one is not optional. The engine's
  runtime-idle point is reached when the *entry module* finishes evaluating,
  which under the test runner is before a single test body has run — so a
  `setTimeout` written inside a `test()` was never drained by it and was
  discarded on the way out, silently. Draining where the test that scheduled it
  is still the current one is also what lets a throwing callback be attributed
  to that test.
- **the engine's idle point**, through the timer extension's `WaitForIdle`,
  which covers timers the entry module itself scheduled.

Real outstanding work outranks virtual time. A timer costs nothing to advance
to, so consulting the queue before polling fetch made
`Promise.race([fetch(url), timeoutAfter(ms)])` resolve to the timeout every
single time, whatever `ms` was, and let a live interval spend a whole budget
before a response could arrive. Fetch and `Atomics.waitAsync` are therefore
polled first, and the clock only moves when nothing real is outstanding.

An exception from a real-mode callback does **not** surface at whichever frame
was waiting. In Node a throwing timer is an uncaught top-level error and the
awaiting frame is untouched; raising it at the wait instead made it catchable by
an unrelated `try` around the `await` and left the awaited promise pending as
well. The queue parks it and the runner attributes it to the test that scheduled
it, reported as `uncaught exception in a timer callback`.

Two consequences are worth stating plainly, because they are divergences from
Node rather than from Vitest:

- **A delay is an ordering key, not a duration.** `setTimeout(fn, 5000)` inside
  an `await` resolves instantly; only the virtual clock moved. This is what
  makes a timer-driven suite fast and reproducible, and it is the whole point.
- **A self-rescheduling timer cannot hang the run.** Every drain is bounded by
  the same 10000-timer limit `runAllTimers` uses. The bound was not
  precautionary: without it, `convex-test`'s scheduler — which re-arms a
  `setTimeout(fn, 0)` after every batch — kept a promise wait alive forever
  during the corpus suite's cleanup hook. A wait that spends the whole budget
  with timers still runnable *names that* rather than reporting an unsettled
  promise, which read as a missing `await` and sent the reader looking in the
  wrong place.
- **Intervals are excluded from the idle drains entirely.** An uncleared one is
  by construction never exhausted, so running it there would spend the whole
  budget and finish no sooner than skipping it — and a throwing one propagating
  out of a teardown path would turn a passing file into a failing one. Whatever
  a callback threw is parked for the host, never raised from a shutdown path.
- **Leftovers do not cross a test boundary.** Whatever a bounded drain did not
  reach is dropped when the test ends, so a strand cannot fire inside the next
  test. Fake-timer state is exempt: that queue belongs to the suite, and Vitest
  does not reset it between tests either.

`useFakeTimers()` installs a fresh clock and discards whatever was pending on
the previous one, which is what a second `useFakeTimers()` does in Vitest. For
GocciaScript that also means a timer scheduled before the switch is dropped
rather than left to a real event loop, because there is no real event loop for
it to be left to.

## Semantics, probed rather than read

Vitest's fake timers wrap `@sinonjs/fake-timers`, so the oracle is the pinned
Vitest 4.1.10 in `scripts/differential/node_modules`, not its documentation.
Each row below was probed against it and is locked in by
`scripts/differential/r-faketimers.test.js`, which vitest gates.

| Behaviour | Probed result |
|---|---|
| `advanceTimersByTime` and microtasks | Runs due timers with **no** microtask draining between them; a promise callback a timer queued waits until the advance returns |
| `advanceTimersByTimeAsync` | Drains microtasks before the first timer and again after each one — `["pending", "first", "after-first", "second", "after-second"]` |
| Ordering of equal due times | Due time, then creation time, then id, so registration order breaks ties |
| Zero, missing or negative delay | Clamped to 0 and due at the current instant |
| Zero delay scheduled *during* an advance | Due at now **+ 1ms**, not now — `delay \|\| (duringTick ? 1 : 0)` — so a zero-delay chain cannot loop inside one advance |
| Interval rescheduling | `callAt += interval` *before* the callback runs, so intervals do not drift; one advance fires every tick it crossed |
| `runAllTimers` with a self-rescheduling timer | Runs exactly 10000 timers, then throws `Aborting after running 10000 timers, assuming an infinite loop!` |
| `runOnlyPendingTimers` | Ticks to the latest due time among the timers pending at the call, so a timer one of them scheduled inside that window fires and one beyond it stays pending |
| `advanceTimersToNextTimer` | `clock.next()` followed by a zero-length tick, so **every** timer due at the instant it landed on fires, not just the first |
| A throwing callback under `advanceTimersByTime` / `runOnlyPendingTimers` | The first exception is recorded, the remaining timers still run, the clock still reaches the requested instant, and the error is rethrown when the advance ends |
| A throwing callback under `runAllTimers` / `advanceTimersToNextTimer` | Stops there — the clock's single-timer step has no handler of its own, so everything behind it stays pending. Probed per member, because the three do not agree |
| `advanceTimersByTime(-1)` | `Negative ticks are not supported` |
| Advancing without fake timers | ``A function to advance timers was called but the timers APIs are not mocked. Call `vi.useFakeTimers()` in the test file first.`` |
| `useFakeTimers()` start instant | The date already in effect — the current fake now if already faking, else a frozen `setSystemTime` date, else real time |
| `useFakeTimers()` called twice | Fresh clock at the current instant; pending timers discarded |
| `setSystemTime` while faking | Moves the wall clock and shifts every pending timer's due time and creation time by the same delta, so remaining delays and ordering are preserved — forwards and backwards |
| `setSystemTime` **without** `useFakeTimers` | Freezes `Date` only; timers and monotonic time are untouched, and `getMockedSystemTime()` reports the frozen date |
| `performance.now()` under fake timers | Elapsed virtual time from the moment fake timers were installed — starts at 0, advances with a tick, and is **not** moved by `setSystemTime` |
| A fractional **delay** | Truncated — the clock computes a due time with `parseInt`, so `setTimeout(fn, 1.5)` is due at 1 and a delay of 0.4 is due immediately |
| A fractional **advance** | Banked, not truncated: `advanceTimersByTime(1.5)` then `(0.5)` moves the clock a full 2ms and fires a timer due there. The pairing with the row above is the opposite of the natural guess, which is why both were probed |
| A `setInterval` with period 0 | Every tick lands on the instant the clock is already on, and the advance still finishes where it was asked to. Node clamps such a period to 1ms; the fake clock does not |
| A nested advance called from inside a timer callback | Sees nothing of the enclosing advance's recorded exception — the record is per operation, not per clock |
| `setSystemTime` with a string | Supported: anything not already a `Date` goes through the `Date` constructor |
| `performance.now()` across the transition | 0 at install, elapsed virtual time while faked, back on the real timeline after `useRealTimers()` |
| `getMockedSystemTime()` | A `Date` while mocked, `null` otherwise |
| `getRealSystemTime()` | The real clock, even while one is mocked |
| Every `vi` timer member | Returns `vi`, so calls chain |
| Fake-timer state between tests | **Not** reset — Vitest leaves the clock installed across tests in a file, and resets only between files |

Two shapes are refused where Vitest admits them, and both refusals exist because
this clock is not a JavaScript number:

- **A non-finite system time.** Vitest lets `setSystemTime(NaN)` — or a string
  `Date` cannot parse — through, and `Date.now()` then reports `NaN` harmlessly.
  Here the mocked clock reaches JavaScript as an `Int64` nanosecond count on the
  host environment, and every consumer of the virtual clock is arithmetic: once
  `NaN` was admitted, every due-time comparison was false, the range test
  selected arbitrarily, and the trailing re-check in the tick recursed on `NaN`
  until the process segfaulted. It is refused at the door instead, in the queue
  rather than at either JavaScript boundary — there are two, and only one of
  them goes through the Vitest shim's `Date` conversion.
- **An advance that can never finish.** A zero-period interval re-arms at the
  instant it just ran, so the clock cannot move past it; Vitest hangs forever.
  The per-advance bound is far above anything a real suite reaches — a 10ms
  interval advanced by an hour fires 360000 times — so it only ever catches that
  shape.

Two shapes were probed and deliberately **not** matched:

- **The timer id.** Vitest runs in Node, where the fake clock hands back a
  Node-shaped `Timeout` object with `ref`/`unref`/`refresh`. GocciaScript hands
  back a number, as the web platform does; it has no Node timer object to
  imitate and no event loop for `ref`/`unref` to mean anything to. `clearTimeout`
  takes either, which is the part suites depend on, and the differential suite
  therefore asserts on clearing rather than on the id's type.
- **`vi.useFakeTimers(config)` beyond `now`.** `toFake` has nothing to select
  from — there is one queue and it is always the faked one — and
  `shouldAdvanceTime` / `advanceTimeDelta` describe real elapsed time, which no
  GocciaScript clock measures. Both are ignored rather than rejected, so a suite
  that passes them still runs.

Three members of the family stay unsupported, and keep the shim's rule of being
a function that throws a named reason rather than an absent property:
`advanceTimersToNextFrame` (no `requestAnimationFrame`, and no display to pace a
frame against), `runAllTicks` (no `process.nextTick` — promise jobs run on the
engine's microtask queue, which the `Async` advance members already drain), and
`setTimerTickMode` (every mode but the default advances against real elapsed
time). Each message says the timer queue itself is present, so the reason points
at the actual gap rather than at a clock that now exists.

## How it is built

`Goccia.Timers` holds the queue: entries, the virtual clock, and the advance
operations. It is shared machinery below both executors, so the interpreter and
the bytecode VM get identical behaviour by construction rather than by
maintenance — the parity requirement ADR 0112 states for the async-context
seams applies unchanged here.

`Goccia.Builtins.Timers` is the JavaScript surface: the four globals plus the
`goccia:timers` module. That module is the low-level control surface and speaks
in numbers — `setSystemTime` takes epoch milliseconds, `getMockedSystemTime`
returns them or `null`. Wrapping those in `Date` and returning `vi` for
chaining is the Vitest shim's job, which keeps the engine surface free of a
dependency on the `Date` shim and leaves `goccia:timers` usable from a suite
that never imports `vitest`.

A mocked clock reaches JavaScript through the engine's host environment
(`TGocciaHostEnvironment.OverrideClock`) rather than by patching a global. That
is the layer `Date`, `Temporal.Now` and `performance` already read — the `Date`
shim is written in JavaScript on top of `Temporal`, so there is nothing to patch
there anyway — and one override keeps every reader consistent. The epoch and
monotonic halves are independent, which is what lets `setSystemTime` outside
`useFakeTimers` freeze the date while leaving `performance.now()` real. It is
not inherited by `ConfigureAsChildOf`, so a ShadowRealm child sees the real
clock until something mocks its own.

`performance.now()` needed one adjustment for this: it normally subtracts its
own time origin, captured from the real monotonic clock at engine boot. A mocked
monotonic clock counts from the mock's own origin instead, so under an override
the elapsed value is used directly. That is what reproduces the probed
behaviour — 0 at install, +250 after a 250ms advance — rather than clamping to 0
forever.

A timer callback is a continuation, so registration captures
`CurrentAsyncContext` and the queue installs it with
`EnterAsyncContext`/`LeaveAsyncContext` around the call, exactly as the
microtask queue does for a job. A pending timer's callback, arguments and
captured snapshot are reachable from nothing else, so the queue publishes them
through a `TGCRootSource` that is rebuilt when the thread's collector changes —
the same rule `Goccia.AsyncContext` follows.

Three properties of that machinery are load-bearing and were each wrong first.

**The queue is a thread singleton, and a realm is not.** A ShadowRealm child
runs on the same thread and shares it, so the drains ask whether the realm
currently executing is the one whose timers the queue carries. Without that, an
`await` inside the child ran the *parent's* callbacks with the child's realm
installed — parent code against child intrinsics, reported by nothing. It is
reachable: an ordinary `async` function suspends and resumes through a promise
reaction, but `Array.fromAsync` awaits on the caller's own stack, so
`realm.evaluate` of that shape reaches the drain.

**A recorded exception belongs to one advance, not to the clock.** A tick drains
microtasks between timers, and guest code reached from there can start another
advance re-entrantly. With a single queue-wide slot the outer tick's exception
was raised at the inner call and the outer tick then believed it had succeeded —
so each operation pushes its own slot and the enclosing ones stay saved, and
stay marked, until it pops. Vitest keeps it per operation too; that was probed
rather than inferred.

**A root source has to be keyed on the collector it is registered with**, not on
one remembered beside it. A `Shutdown`/`Initialize` pair can put the next
thread-local collector at the address the previous one had, and a bare pointer
compare then reports "same collector" for a source registered with the dead one,
leaving everything it publishes unmarked. `TGCRootSource.RegisteredCollector`
reads the registration the collector's destructor nils, which cannot match a
destroyed one. `Goccia.AsyncContext` had the same latent compare and is fixed
with it.

## Availability

The timer globals and `goccia:timers` are installed by the **test-runner
profile only**, not by the loader profile. They are deterministic and carry no
ambient authority — no I/O, no real clock, no way to observe anything the
program did not already have — but they are still a scheduling surface a
sandboxed script does not otherwise get, and the acceptance target for them is
the runner. Widening this to `GocciaScriptLoader` is a later decision with its
own evidence; nothing in the design depends on where the extension is
installed.

Per-file isolation comes from the runner's existing lifecycle rather than from a
reset hook: `GocciaTestRunner` builds a fresh engine per test file, and the
extension clears the queue when it attaches. Per-*test* state is deliberately
not reset, because Vitest does not reset it either.

## Consequences

`vi.waitFor` and `vi.waitUntil` still throw, and their message had to be
rewritten rather than deleted. They are async polling APIs, not timer APIs: each
needs execution to suspend and resume between attempts, and `await` in
GocciaScript is a synchronous drain. The old message pointed at a missing fake
clock, which is no longer the gap; the new one names the suspension point and
the fact that the virtual clock only moves when a test moves it.

Adding the timer step to `WaitForFetchPromise` widens what that function is,
and its name now under-describes it. It was already the host's general
"drive this promise to settlement" wait — the test runner, `expect().resolves`
and `expect().rejects` all call it — so the alternative was a second, nearly
identical wait beside it. The comment at the call site records the widening.
