# Non-strict this coercion

**Date:** 2026-05-15
**Area:** `engine`
**Pull Request:** [#645](https://github.com/frostney/GocciaScript/pull/645)

Completed the non-strict compatibility surface from #586 by making regular function calls coerce nullish `this` to `globalThis` behind `--compat-non-strict-mode` in interpreter and bytecode modes. Bytecode function templates now serialize their strict-this mode, the bytecode format is v30, the CLI/config tests cover the behavior, and the arguments-object helper lives in the value namespace as `Goccia.Values.ArgumentsObjectValue`. [language.md](../language.md#this-binding-strict-mode-semantics). [bytecode-vm.md](../bytecode-vm.md#compatibility-scope-helpers). [value-system.md](../value-system.md).
