# Unmanaged execution-context records

**Date:** 2026-08-26
**Area:** `runtime`, `bytecode`

`TGocciaExecutionContext` stays an unmanaged record. Source paths are interned
to stable `UnicodeString` nodes and stored as pointers, so `Push`/`Pop` copy
only pointers instead of `FPC_COPY` of a managed string on every call.

Richards host samples of the production bytecode loader were dominated by
`SetupNewFrame` / `TeardownCurrentFrame` paying `FPC_COPY` because the context
record still carried `SourcePath: string`. After this change, interleaved AWFY
on darwin/aarch64 moved Richards about 18% versus the previous stack tip, and
the eight-guard geomean `goccia_over_qjs` from about 15.0 to 14.1. The reader
API is unchanged: `SourcePath` remains a string property.

This is not guest-string interning. [ADR 0013](0013-reject-string-interning.md)
rejected content-keyed caches of `TGocciaStringLiteralValue` on `RuntimeCopy`.
The intern here is a native, thread-local list of diagnostic file paths so the
execution-context record can stay unmanaged. It does not sit on `RuntimeCopy`
and does not intern guest strings.

Making the diagnostic `TGocciaCallFrame` unmanaged the same way was measured
on the same host and rejected: Richards did not move (median ratio 0.9998).
[bytecode-vm.md](../bytecode-vm.md#performance-direction).
