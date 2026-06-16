# Reject string interning

**Date:** 2026-02-20
**Area:** `strings`

No string interning. Dictionary-based string interning was benchmarked at −4% across 172 benchmarks. FPC's COW semantics make allocation effectively free; the hash + lookup cost exceeds it. [core-patterns.md § String Interning](../core-patterns.md#string-interning--attempted-and-rejected).
