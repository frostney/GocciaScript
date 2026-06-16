# TStringBuffer over TStringBuilder

**Date:** 2026-03-11
**Area:** `strings`
**Pull Request:** [#65](https://github.com/frostney/GocciaScript/pull/65)

TStringBuffer over TStringBuilder. Both `TUnicodeStringBuilder` and `TAnsiStringBuilder` trigger a 750× slowdown from FPC's heap manager without preallocation. `TStringBuffer` (advanced record with doubling growth) is ~2× faster even with preallocation. [#65](https://github.com/frostney/GocciaScript/pull/65). [spikes/fpc-string-performance.md](../spikes/fpc-string-performance.md).
