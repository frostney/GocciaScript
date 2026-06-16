# VMT dispatch for AST evaluation

**Date:** 2026-03-08
**Area:** `interpreter`
**Pull Request:** [#51](https://github.com/frostney/GocciaScript/pull/51)

VMT dispatch on AST nodes. Expression and statement evaluation uses virtual dispatch instead of `if ... is` type check chains. Eliminates `TObject.InheritsFrom` overhead (18.4% of interpreted instructions in callgrind profiling). [#51](https://github.com/frostney/GocciaScript/pull/51). [core-patterns.md § Virtual Dispatch Value System](../core-patterns.md#virtual-dispatch-value-system).
