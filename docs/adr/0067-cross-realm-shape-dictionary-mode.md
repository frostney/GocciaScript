# Cross-realm shape dictionary mode

**Date:** 2026-06-13
**Area:** `bytecode`

Shape-lite property maps now record their owner realm and switch to dictionary mode when `EnsureShape` runs from any other realm. This keeps `$262.createRealm` cross-realm property reads from interning one engine's object layout into another realm-owned shape table while preserving the single-realm cache-hit path as a raw shape-field read. [#751](https://github.com/frostney/GocciaScript/issues/751). [bytecode-vm.md § Inline Caches](../bytecode-vm.md#inline-caches).
