# Opt-in non-strict compatibility

**Date:** 2026-05-15
**Area:** `engine`
**Pull Request:** [#645](https://github.com/frostney/GocciaScript/pull/645)

Added opt-in ES non-strict compatibility via `--compat-non-strict-mode` / `"compat-non-strict-mode"` for `arguments` objects, `with` statement execution, and legacy `delete` return values in both interpreter and bytecode modes. When enabled, ordinary functions, methods, accessors, and generators receive an unmapped `arguments` object while arrows keep lexical lookup; `arguments` stays an ordinary identifier that can be shadowed. `with` uses object environment lookup with `Symbol.unscopables`, identifier calls preserve the object as `this`, bytecode captures hidden with-object locals so closures created inside `with` keep resolving correctly, and `delete` returns `false` for non-deletable bindings/properties instead of throwing. [language.md](../language.md#arguments-object). [language.md](../language.md#with-statement). [language.md](../language.md#delete-semantics). [interpreter.md](../interpreter.md#scope-chain-design). [bytecode-vm.md](../bytecode-vm.md#compatibility-scope-helpers).
