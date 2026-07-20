# Native binary64 execution with narrow semantic operations

**Date:** 2026-07-19
**Area:** `runtime`

GocciaScript continues to represent ECMAScript Number values as the supported compilers' native binary64 `Double`, retaining ADR 0016's single IEEE-754 representation and rejecting a parallel arithmetic backend. Compilation and execution enter a bounded floating-point scope that saves the host state, establishes the required rounding and exception behaviour, and restores the exact inherited state in `finally`; this supersedes ADR 0016's engine-lifetime exception-mask guard and forbids permanent process-wide FPU mutation.

Ordinary finite arithmetic uses native Pascal operators and RTL functions. Shared ECMAScript operations add only specification-mandated guards or probe-demonstrated compatibility logic, consistently across compilers. Raw Float16, Float32, and Float64 reinterpretation is owned by `NumberBits`; canonical stored NaNs are `0x7E00`, `0x7FC00000`, and `0x7FF8000000000000` respectively. Numeric text conversion follows ADR 0098. `Number::remainder` is one shared, bit-exact Pascal semantic leaf because neither Pascal `mod` nor the supported RTL `FMod` provides the required binary64 contract across the target matrix; interpreter evaluation, constant folding, and bytecode execution call it directly. The same direct-caller rule applies to other shared numeric semantic leaves such as exponentiation and number conversion.

The engine does not import platform `libm` or another shared/static numerical library to define ECMAScript semantics. When an RTL operation is absent or observably incompatible, GocciaScript uses explicit ECMA-262 guards around a common RTL finite path or a shared pure-Pascal implementation. Compiler-specific branches may adapt syntax or primitive names but may not select different Number semantics. The normative baseline is ECMA-262 ES2026 snapshot `0248456c758431e4bb8e5d26333ff1865123c9cd`, including §6.1.6.1.6 `Number::remainder`.
