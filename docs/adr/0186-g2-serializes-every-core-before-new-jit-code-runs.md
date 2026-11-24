# 0186 - G2 serializes every core before code its JIT wrote runs

**Date:** 2026-11-22
**Area:** `g2`, `jit`
**Related:** [ADR 0176](0176-the-interpreter-beats-quickjs-before-gcbundle-a-jit-and-a-compiled-executable.md)

## Context

G2's baseline JIT and its optimizing tier write machine code into executable
chunks and run it right away, on the thread that compiled it. x86 lets a core
run instructions it fetched before another core wrote over them, until that
core executes a serializing instruction (Intel SDM vol. 3A, "Handling Self-
and Cross-Modifying Code"; AMD64 APM vol. 2, "Cross-Modifying Code"). A
thread can compile on one core and run the code on another. Linux switches
between two threads of one process without writing CR3, so the switch
serializes nothing.

`GocciaTestRunner --jobs=2` hit this in about 1 run in 150: two test files
ran at once, and the compiling thread ran a fresh template's prologue as the
chunk's old zeros (`00 00`, `add [rax], al`) followed by `push rsp` where
`push rbx; push r12` belonged. The frame then came out one slot short, and the
epilogue returned to a stale stack slot. It never happened with one job or with
the JIT off.

## Decision

Every write of code goes through `JitPublish`. It copies the code, runs CPUID
on the writing core, and on Linux calls `membarrier` with
`MEMBARRIER_CMD_PRIVATE_EXPEDITED_SYNC_CORE`. The process registers for that
command once, when the VM unit starts. After the call every core that runs one
of the process's threads has serialized, and the kernel serializes the others
before they next return to the process, so the compiling thread can move to any
core.

## Consequences

- The call costs about 0.7 µs per compile in a single-threaded process and
  about 2 µs with four threads running (measured on the development machine).
  The typescript Web Tooling bundle compiles about 1,750 templates, about
  1.2 ms of a 0.49 s run.
- Where `membarrier` is missing (not Linux, or a kernel without `SYNC_CORE`)
  only the writing core serializes, and a thread that moves between the write
  and the run keeps the old hazard.
- Code written after compilation, such as patched call sites, would need the
  same barrier. The JIT patches nothing after `JitPublish` today.
