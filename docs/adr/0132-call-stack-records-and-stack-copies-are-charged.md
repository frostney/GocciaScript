# 0132 - The call-stack records and the copy a stack growth makes are charged with the VM stacks

**Date:** 2026-10-05
**Area:** `gc`, `bytecode runtime`, `sandbox`
**Related:** [ADR 0130](0130-vm-stacks-are-charged-to-the-memory-budget.md), [ADR 0129](0129-heap-triggered-collection.md), [ADR 0106](0106-sandbox-hardening-scope.md), [#1504](https://github.com/frostney/GocciaScript/issues/1504), [#1503](https://github.com/frostney/GocciaScript/issues/1503)

## Context

ADR 0130 charges the bytecode VM's register, local-cell, argument, frame and
closed-numeric-frame stacks to `--max-memory`, so that with `--max-stack=0` a
recursion stops with a catchable `RangeError` at the ceiling. At that refusal
the process still held about 2.2 times the ceiling. Three costs outside the
charge made up the gap:

- **The records the VM pushes for every call.** Each call pushes a frame
  record onto the thread's call stack (`TGocciaCallStack`, 40 bytes) and a
  context onto its execution-context stack (48 bytes). A frame inside a `try`
  also pushes an entry onto the VM's handler stack (16 bytes). Beside the 175
  bytes the recursion in #1472 charges a frame, these added half as much
  again, uncharged, and none of them shrank.
- **The copy.** `SetLength` allocates the grown block, copies the old one into
  it and only then frees it, so for a moment a stack holds its old and its new
  capacity. A growth of the largest stack briefly added that stack's whole
  size again.
- **The `RangeError`'s stack text**, which listed every frame. #1503 caps it at
  100 frames.

## Decision

ADR 0130's contract stands: the stacks are charged, a refusal is the catchable
`RangeError` decided at the next instruction boundary after a collection, a
growth inside call setup neither collects nor throws, and a charge is released
against the collector that took it. This decision widens what is charged and
adds one rule to how a stack may grow.

- **The VM grows the thread's call stack and execution-context stack, and its
  own handler stack, through the charged growth path of its other stacks.** The
  pushes no longer grow: `TGocciaCallStack.PushTemplate`,
  `TGocciaExecutionContextStack.PushFunctionContext`,
  `TGocciaBytecodeHandlerStack.Push` and `RestoreFrom` expect room, and the VM
  checks for it before each push, where those pushes used to check. A frame
  the VM pushes for a native constructor call grows the call stack the same
  way. Each of the three stacks grows to 64 entries uncharged, like the VM's
  own initial capacities; past that it is charged, and it shrinks back with
  the other stacks at the instruction boundary.
- **The thread's stacks give back no more than the VM charged for them.** The
  call stack and the execution-context stack belong to the thread, and other
  code, such as the interpreter, grows them uncharged. The VM records how many
  bytes of each it accounted for, and for which thread's stack. A shrink
  releases at most that, so it never releases the charge of another stack. If
  the VM later runs on another thread, its charge for the first thread's
  stacks stays until the VM is destroyed, because that memory is still
  allocated. When the VM is destroyed on the thread it ran on, it also shrinks
  that thread's stacks: a recursion that ended in an uncaught error leaves no
  instruction boundary to shrink them at, and the next engine on the thread
  would otherwise use them uncharged.
- **Every stack is charged for its capacity.** ADR 0130 already charged the
  capacity each growth adds; the three stacks above are charged the same way.
- **A growth must leave room under the ceiling to copy the largest stack once
  more.** A growth of G bytes leaves the stacks G bytes bigger and the largest
  stack at most G bytes bigger, so it is taken only while
  `2G + largest <= MaxBytes - BytesAllocated`. That covers the copy this
  growth makes and the next one, charged or not: a growth that does not fit
  still takes the entries its frame needs, uncharged, and copies the stack to
  do so. The settle at the instruction boundary applies the same rule to the
  quarter of growth room it requires. The copy room is measured against the
  ceiling itself, not the memory-pressure line, because the copy is transient
  and nothing else is allocated while it is held.

## Consequences

The recursion from #1472, `const f = () => { d++; f(); }`, production build,
Linux x86-64, `--mode=bytecode --max-stack=0`, every run capped at 4 GB.
Peak RSS is from `/usr/bin/time`; an idle run of either binary holds
11.1 MiB. "Before" is main at 9e1f766d, which caps the `RangeError`'s stack
text at 100 frames (#1503):

| `--max-memory` | Before: depth | Before: peak RSS | After: depth | After: peak RSS |
|---|---:|---:|---:|---:|
| 64 MiB | 331,783 | 143.2 MiB (2.24x) | 175,510 | 78.3 MiB (1.22x) |
| 256 MiB | 1,401,400 | 563.9 MiB (2.20x) | 703,530 | 281.1 MiB (1.10x) |
| 1 GiB | 6,143,728 | 2,266.8 MiB (2.21x) | 2,815,609 | 1,083.9 MiB (1.06x) |

Above the idle run, the peak is now 1.05 times the ceiling at all three
sizes.

The recursion stops at about half the depth: each frame is now charged about
260 bytes rather than 175, and the stacks keep room to copy the largest of
them.

What is left above the ceiling is garbage. `BytesAllocated` counts a dead
value at its `InstanceSize`, and the heap manager holds more for it (ADR 0129
measured 2.4 to 3.6 times). In this script each frame drops a number, and
between collections those take a few percent of the ceiling more than they
are counted for. That gap is #1467's, outside this decision. Memory a stack
gives back when it shrinks stays resident in the heap manager, so two deep
recursions in one run can still add up past the ceiling.

`scripts/test-cli-apps.ts` bounds the recursion's peak RSS at an idle run's
plus 1.3 times the 64 MiB ceiling, which the code before this decision
exceeds, and asserts that a recursion through `try` blocks is charged for its
handlers and gives them back.

Callgrind instruction counts on the AWFY rows and the `perf/probes` set move
by at most 0.04%, except `typed-array-update-loop`, 0.13% lower, and
`fib-recursive`, 0.12% higher. There a numeric self-call costs one more
check, now that the VM, rather than `PushTemplate`, checks the call stack for
room.
