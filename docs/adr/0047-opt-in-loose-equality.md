# Opt-in loose equality

**Date:** 2026-05-11
**Area:** `engine`

Added opt-in loose equality compatibility (`--compat-loose-equality` / `"compat-loose-equality"`). `==` and `!=` remain warning-producing no-ops by default, but when enabled they use the shared ES2026 `IsLooselyEqual` implementation in interpreter and bytecode modes, including string/number/boolean coercion, BigInt mixed comparisons, and object `ToPrimitive`; the bytecode format is bumped to v27 for `OP_LOOSE_EQ` and `OP_LOOSE_NEQ`. [language.md](../language.md#loose-equality--and-).
