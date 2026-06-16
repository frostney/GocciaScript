# TOrderedStringMap for scope bindings

**Date:** 2026-03-18
**Area:** `data-structures`
**Pull Request:** [#66](https://github.com/frostney/GocciaScript/pull/66)

TOrderedStringMap for scope bindings. A `TScopeMap` with linear scan was attempted and abandoned after profiling showed 2.7× slowdown vs hash-based lookup. [#66](https://github.com/frostney/GocciaScript/pull/66). [spikes/fpc-hashmap-performance.md](../spikes/fpc-hashmap-performance.md).
