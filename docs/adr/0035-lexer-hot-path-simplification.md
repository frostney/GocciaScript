# Lexer hot path simplification

**Date:** 2026-04-29
**Area:** `parser`

Lexer hot paths favor direct scanning and measured tiny-method `inline` hints over abstraction in the numeric scanner. Comment skipping, block comments, regex literal slicing, and template line-terminator handling were simplified and benchmarked with `GocciaBenchmarkRunner` lex timing; the numeric separator helper was rejected because it shortened the code but regressed decimal and radix-heavy cases. [spikes/lexer-performance-simplification.md](../spikes/lexer-performance-simplification.md).
