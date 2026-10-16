# Scope creation via CreateChild

**Date:** 2026-02-16
**Area:** `interpreter`

`CreateChild` factory for scopes. Direct `TGocciaScope.Create` replaced with `ParentScope.CreateChild(kind)` to ensure proper parent linkage and callback propagation. [architecture.md § Scope Chain Design](../architecture.md#scope-chain-design).
