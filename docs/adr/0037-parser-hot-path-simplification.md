# Parser hot path simplification

**Date:** 2026-04-29
**Area:** `parser`

Parser hot paths now avoid repeated cursor-helper and chained `Match(...)` dispatch in `Advance`, `Check`, `Match`, remaining expression operators, call/member continuations, and primary expressions. Same-corpus production timing with `GocciaScriptLoader --output=json` showed 35.1-49.1% parser-phase improvements versus clean `HEAD`, so this supersedes the narrower binary-only parser optimization while keeping recursive descent and avoiding parser unit splits. [spikes/parser-hot-dispatch-performance.md](../spikes/parser-hot-dispatch-performance.md).
