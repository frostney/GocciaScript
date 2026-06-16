# Unified heap object hierarchy

**Date:** 2026-03-25
**Area:** `bytecode`
**Commit:** `ac2a9bc1`

Unify heap object hierarchy. `TGocciaValue` now inherits from `TGocciaCompiledHeapObject`, removing the bridge between interpreter and bytecode value models. [bytecode-vm.md § Design Rationale](../bytecode-vm.md#design-rationale).
