# 0169 - Win32 builds add audited SafeSEH metadata after linking

**Date:** 2026-10-29
**Area:** `build`, `windows`, `exceptions`

## Context

FPC's i386 Win32 executables lack a SafeSEH handler table. In the measured
Windows exception path, validation therefore queries virtual memory for each
handler frame. Deep Proxy recursion spends seconds processing its RangeError;
the same executable with the table completes the file in milliseconds.
[The controlled Windows run](https://github.com/frostney/GocciaScript/actions/runs/40958622457)
kept every original code and data section unchanged.

A linker map supplies symbol addresses but does not classify every exception
handler. RTL code also replaces handlers through pointers to existing frames;
searching only for writes to `FS:[0]` misses those registrations. In addition to
the core `system` handlers, `signals` installs its own exception handler.

## Decision

Use a supported Pascal post-link step shared by the local build and CI for every
i386-win32 executable. Keep the compiler and toolchain unchanged. Resolve
handler addresses from the executable's map against a reviewed manifest keyed
to the exact FPC version and SHA-256 of audited RTL objects defining or
registering handlers. Account for optional linked units explicitly.

Unknown handler-like symbols, missing required symbols, changed audited objects,
and unsupported image layouts fail the build. Accept only fixed-base, unsigned
PE32 executables without existing load configuration. Append a read-only
load-configuration directory and sorted handler RVAs, preserving original
section bytes. Signing happens afterward. Win64 already uses table-based
unwinding and needs no post-link change.

## Consequences

Released Win32 binaries receive the performance improvement as well as CI.
Toolchain changes require a new registration audit and manifest review.
This is a contract for the reviewed FPC inputs, not a general-purpose PE linker
or a claim that arbitrary third-party object code is safe.

CI verifies the metadata in every staged executable and exercises exception
paths on real Windows. Controlled copies with the directory reference removed
provide matching baseline behavior and timing. The existing full suite, timeout,
recursion bounds, and exception semantics remain in force.

[Win32 exception metadata](../contributing/win32-safeseh.md) owns the build and
re-audit procedure.
