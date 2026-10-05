# 0131 - A bytecode caller frame's position is worked out when a stack is captured, from the instruction pointers the VM already saves

**Date:** 2026-10-05
**Area:** `bytecode runtime`, `diagnostics`
**Related:** [ADR 0014](0014-bytecode-and-interpreter-feature-parity.md), [ADR 0074](0074-deferred-bytecode-call-stack-frames.md), [ADR 0126](0126-bytecode-call-path-arena-fills-and-thread-binding.md)

## Context

ADR 0074 pushes a bytecode frame onto the shared call stack as a bare
template pointer with no position, so that a call does no stack-trace work.
A frame got a position only when the VM stamped one on it: on a throw path,
around a native call, and around `new`. Any other caller frame, including
the top level, read `file:0:0` in every stack trace
([#1494](https://github.com/frostney/GocciaScript/issues/1494)). Once the
tree-walk interpreter is removed (#825), every stack trace comes from the VM.

Stamping each frame at its call site would put a call-site lookup and string
assignments on every call. ADR 0074 rejected the opposite extreme: rebuilding
traces entirely from the VM's frame stack. That stack lacks native and
constructor frames, and `ASkipTop` counts those frames.

## Decision

Positions are worked out only when a trace is captured, by the
`Error` constructor or a throw. The pushes stay as ADR 0074 left them.

- Each native entry into the dispatch loop (`ExecuteClosureRegistersInternal`)
  records a `TGocciaVMActivation`: the call stack's count when the entry
  began, its first frame-stack and closed-numeric-frame slots, and pointers
  to its `Template` and `InstructionStartIP` locals. These are the same
  probes `StampThrowLocation` already used, kept in the array instead of in
  two saved locals.
- Each frame an entry runs pushed one deferred call-stack frame. For every
  frame but the executing one, the VM saved an instruction pointer when that
  frame made its call: in the frame stack, then in the closed numeric frame
  stack. The resolver pairs the two sequences in order, entry by entry. It
  steps back from each saved pointer to the call instruction, allowing for
  an `OP_WIDE` prefix, and looks the instruction up in the call-site table,
  falling back to the line map. The executing frame is located at the
  instruction its probe points at. A pairing that does not match leaves its
  frames unlocated; a frame is never located by guesswork.
- A stamp still wins. The resolver fills in only deferred frames with no
  stamp, so the throwing frame and the call-site stamps around native calls
  keep their existing positions.
- `TGocciaCallStack` stays independent of the bytecode units. It calls a
  `TGocciaFrameLocationResolver` method pointer, which the outermost VM entry
  on a thread installs and restores when it returns. A VM nested inside
  another hands the outer VM's frames back to the outer resolver.

## Consequences

- A call does no position work. A native entry stores one record, and a
  capture walks the stack once.
- Caller frames, the top-level `<module>` frame, an imported module's top
  level and the frames of a numeric recursion all report the call they are
  making. A failed dynamic `import()` gets a code frame.
- A frame is located at its call expression's recorded position: the
  opening parenthesis of a call, and `new` for a construction. V8 points at
  the start of the callee instead. #1494 tracks that column.
- Frames that another engine pushed below a VM's outermost entry are located
  only through that engine's own resolver.
