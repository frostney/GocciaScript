# GocciaScript bytecode VM unification

**Date:** 2026-03-23 → 2026-04-10
**Area:** `bytecode`
**Pull Requests:** [#107](https://github.com/frostney/GocciaScript/pull/107) [#109](https://github.com/frostney/GocciaScript/pull/109) [#110](https://github.com/frostney/GocciaScript/pull/110) [#136](https://github.com/frostney/GocciaScript/pull/136) [#137](https://github.com/frostney/GocciaScript/pull/137) [#254](https://github.com/frostney/GocciaScript/pull/254)

VM unification. Fold the separate Souffle VM into a GocciaScript-specific bytecode VM: eliminate interpreter bridges for modules (#107) and async/await (#110), add native-value-backed Map/Set (#109), refactor built-in object model registration (#136), fold the VM directly (#137), and extract the GC into a standalone `Goccia.GarbageCollector` unit (#254). [bytecode-vm.md § Design Rationale](../bytecode-vm.md#design-rationale). [garbage-collector.md](../garbage-collector.md).
