# 0182 - Linux thread stack bounds come from pthread_getattr_np

**Date:** 2026-11-21
**Area:** `vm`, `platform`
**Related:** [ADR 0027](0027-heap-trampoline-call-stack-limit.md), [Embedding — Call Stack Depth Limit](../embedding.md#call-stack-depth-limit)

## Context

`Goccia.StackLimit` keeps `NATIVE_STACK_RESERVE` of a thread's stack free, or a
quarter of a stack smaller than four reserves, so it needs the bounds of the
running thread's stack. On Linux it read them from the mapping in
`/proc/self/maps` that holds the running frame. That needed neither libc nor
libpthread, and libpthread was deliberately not linked: glibc before 2.34 keeps
`pthread_getattr_np` there, and not every program linked it.

glibc 2.42 and later, on Linux 6.13 and later, install a thread's guard page
inside its stack's mapping with `madvise(MADV_GUARD_INSTALL)` instead of as a
`PROT_NONE` mapping of its own. Two thread stacks that lie next to each other
then have nothing between them, and the kernel merges them into one mapping.
`/proc/self/maps` can no longer tell a thread's own stack from a neighbour's: a
512 KiB stack mapped just below a 4 MiB one reads as 4.5 MiB and kept the full
256 KiB reserve. A mapping merged below a thread's stack moves the bottom into
the neighbour's stack instead. The RTL's `StackBottom` cannot stand in for the
bottom either, because it lies below the real one by whatever the thread used
before the RTL started it, such as its static TLS. Ubuntu 26.04 ships glibc
2.43, and a test of the quarter rule failed intermittently there.

## Decision

On Linux, a thread other than the main one takes its stack's bounds from the
thread library: `pthread_getattr_np` and `pthread_attr_getstack`. glibc
answers from its own record of the stack it gave the thread and leaves the
guard pages out, so the reserve starts above the guard and no merged mapping
affects it. The unit names libpthread for these functions, so every Linux
program that uses it links libpthread, as the `cthreads` unit already does; on
glibc 2.34 and later the symbols resolve in libc.

The main thread keeps the `/proc/self/maps` and `RLIMIT_STACK` path, which
glibc's own `pthread_getattr_np` uses for it too, and so does a thread whose
bounds the thread library cannot give. Darwin and Windows are unchanged.

## Consequences

- A thread's reserve no longer depends on where the kernel places its stack.
- Every Linux program that uses `Goccia.StackLimit` links libpthread.
