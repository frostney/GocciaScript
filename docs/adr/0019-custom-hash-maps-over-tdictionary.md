# Custom hash maps over TDictionary

**Date:** 2026-03-18
**Area:** `data-structures`
**Pull Request:** [#66](https://github.com/frostney/GocciaScript/pull/66)

Custom hash maps over TDictionary. Purpose-built `TOrderedStringMap`, `THashMap`, and `TOrderedMap` replace `TDictionary` on hot paths with 4–6× faster inserts at typical sizes. [#66](https://github.com/frostney/GocciaScript/pull/66). [spikes/fpc-hashmap-performance.md](../spikes/fpc-hashmap-performance.md).
