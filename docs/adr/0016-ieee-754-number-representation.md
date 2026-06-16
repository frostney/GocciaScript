# IEEE 754 number representation

**Date:** 2026-03-05
**Area:** `runtime`
**Pull Request:** [#39](https://github.com/frostney/GocciaScript/pull/39)

Switch number representation from enum to IEEE 754. Removed `TGocciaNumberSpecialValue` enum and `FSpecialValue` field from `TGocciaNumberLiteralValue`. Numbers now store a single `Double` using standard IEEE 754 bit patterns for NaN, Infinity, and -0. Enabled by masking all FPU exceptions via `SetExceptionMask` so that operations like `0.0 / 0.0` produce IEEE 754 NaN instead of raising a Pascal exception. The `IsNaN`/`IsInfinity`/`IsNegativeZero` property accessors delegate to `Math.IsNaN`/`Math.IsInfinite` and an endian-neutral sign-bit check. The engine saves and restores the previous exception mask on creation/destruction. [#39](https://github.com/frostney/GocciaScript/pull/39). [value-system.md § Number Representation](../value-system.md#number-representation).
