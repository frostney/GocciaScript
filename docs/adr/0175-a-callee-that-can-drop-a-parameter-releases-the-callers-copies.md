# 0175 - A callee that can drop a parameter releases the caller's copies of its arguments

**Date:** 2026-11-03
**Area:** `vm`, `gc`
**Related:** [ADR 0126](0126-bytecode-call-path-arena-fills-and-thread-binding.md), [#2033](https://github.com/frostney/GocciaScript/issues/2033), [#2027](https://github.com/frostney/GocciaScript/issues/2027)

## Context

`OP_CALL` and `OP_CALL_METHOD` read a bytecode callee's arguments straight out of
the caller's registers, and `SetupNewFrame` copies them into the callee's
registers. The caller's copies stay, and the collector marks the caller's whole
register window, so a value stays reachable for the whole call even after the
callee drops its own reference to it:

```js
const f = (b) => { b = null; Goccia.gc(); /* allocate: refused */ };
f(new ArrayBuffer(150000000));
```

A promise reaction job has the same shape: the running job roots its argument as a
queued root and holds it in an arguments collection until the handler returns.
[#2027](https://github.com/frostney/GocciaScript/issues/2027) released the root for
an `await` continuation only.

The owner decided to fix this rather than leave it as a known cost of the call
convention.

## Options

1. **Clear inside `SetupNewFrame`, after it copies the arguments.** One place, but
   `SetupNewFrame` does not know where `AArguments` points. A tail call's
   arguments sit in the register window of the frame being replaced, above the
   registers they are copied to, and the copy relies on that ordering: clearing
   the source after the copy can clear another argument's destination. Native
   callers (`Array.prototype.map` and the like) pass an array they reuse and read
   afterwards. Both would need a flag on a procedure every call goes through.
2. **Clear in the dispatching opcode, after `SetupNewFrame`.** The opcode knows
   the arguments are its own consecutive temporaries (the compiler allocates a
   fresh register per argument and frees them after the call), and it knows the
   call is not a tail call. Native callers never reach this code.
3. **Clear after every call in the compiler's output** (an `OP_LOAD_UNDEFINED`
   per argument register after each `OP_CALL`). Costs an instruction per
   argument per call on every call, including the ones that return at once.
4. **Callee-side: the callee clears its caller's window.** Reaches into another
   frame's registers from `SetupNewFrame` with the same problems as option 1.

## Decision

**Option 2, and only when it can matter.**

- After `SetupNewFrame` of a non-tail, non-spread bytecode callee, `OP_CALL` and
  `OP_CALL_METHOD` set the caller's argument registers that hold an object to
  undefined (`ReleaseCallerRegisters`, indexed from the register base captured
  before the callee's frame replaces it).
- Only for a callee whose template says it can write a parameter
  (`ParametersMayBeDropped`). The caller's copy matters only once the callee has
  dropped its own: a callee that never writes a parameter holds the value in its
  parameter register for as long as it runs, so there is nothing to gain. The
  compiler sets the flag to false when every parameter is a plain identifier with
  no default and the body holds no assignment to an identifier, no increment or
  decrement, no destructuring assignment, no direct eval, no suspension point and
  nothing the `Goccia.Compiler.OperandSafety` allowlist does not name
  (`ParametersSurviveBody`, which reuses the walk behind the operand proofs). The
  flag is not serialised: a template loaded from `.gbc` keeps the default, true,
  and always releases.
- For a promise reaction job, `TGocciaFunctionBase.CallOneArgOwned` calls a
  bytecode handler with the settled value. The handler's first frame, once it
  holds the argument, tells the queue to drop the job's root
  (`TGocciaVM.HandOffOwnedArgument`, `ReleaseRunningJobArgument` from #2027), and
  no arguments collection holds the value. A callee that sets up no
  frame leaves the root to the job. A generator with a parameter preamble runs
  the preamble in its first frame and hands off too, which is safe because the
  generator object keeps the argument itself. A handler that is not a bytecode
  function keeps the previous path.

## Consequences

Per-call cost, measured as callgrind instruction counts of the production
`GocciaRunner` over one million calls (fib: `fib(25)`, 242,785 calls), before and
after:

| Workload | Base | After | Change |
| --- | ---: | ---: | ---: |
| 3 numeric args, callee never writes a parameter | 3,711.1 M | 3,723.1 M | +0.32% |
| 3 object args, callee never writes a parameter | 4,810.8 M | 4,822.8 M | +0.25% |
| method call, 2 args | 4,345.8 M | 4,357.8 M | +0.28% |
| recursive `fib(25)` | 344.2 M | 347.1 M | +0.85% |
| 3 object args, callee reassigns all 3 | 4,788.7 M | 4,852.7 M | +1.34% |
| `Array.prototype.map` callback, 1,000,000 entries from native code | 8,064.9 M | 8,068.9 M | +0.05% |

Twelve instructions per call for every non-tail bytecode call (the template flag
and the saved index), about 17 more per released object argument, and four per
entry from native code (the pending-argument test). A callee that
reassigns its parameters pays the release; the rest pay the flag test.

- A callee that reassigns a parameter, and `.then(cb)` handlers that drop their
  parameter, no longer keep the value alive through the caller. A callee that
  keeps its parameter keeps the value, as it must.
- Native callees, spread calls, bound-function calls, tail calls, async callees,
  `new C(x)` and the `f.call` / `f.apply` fast paths are unchanged. A caller's
  argument registers stay populated after them until reused.
- A callee that only forwards its parameter in a tail call (`(b) => g(b)`) never
  writes it, so its flag is false and the caller's copy stays while `g` runs. The
  alternative, a true flag for every body that returns a call, makes the common
  wrapper pay about 17 instructions per object argument; that is the cost the
  gate exists to avoid.
- The registers cleared are temporaries nothing reads again: the compiler frees
  them right after the call instruction.
