# Eliminate global mutable state

**Date:** 2026-02-16
**Area:** `interpreter`

Eliminate global mutable state. All runtime state flows through `TGocciaEvaluationContext`, the scope chain, and value objects. The `GlobalEvaluationContext` mutable global was removed. [interpreter.md § Pure Evaluator Functions](../interpreter.md#pure-evaluator-functions).
