# Direct binary precedence parsing

**Date:** 2026-04-29
**Area:** `parser`

Binary-expression precedence parsing now uses direct `Peek.TokenType` loops on the measured hot precedence layers instead of routing through the generic open-array `ParseBinaryExpression` helper. Production timing showed 3.6-18.9% parser-phase improvements across larger synthetic parser maps and 6.3-14.3% improvements across existing benchmark-file parse medians; rejected alternatives included explicit `Advance inline`, eager source-text removal, and repeated-`Check(...)` loops. [spikes/parser-binary-expression-performance.md](../spikes/parser-binary-expression-performance.md).
