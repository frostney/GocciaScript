# Opt-in function declarations

**Date:** 2026-04-23
**Area:** `parser`

Opt-in `function` declarations (`--compat-function`). Added `function` declaration and expression support behind a `--compat-function` CLI flag for ECMAScript compatibility when porting legacy code. Function declarations desugar to var-scoped bindings backed by `TGocciaFunctionExpression`, which produces call-site `this` binding (not lexical). Declarations are hoisted per ES spec: both name and value are available before the declaration is reached, using the same `DefineVariableBinding` infrastructure as `--compat-var`. Async functions are supported; generators (`function*`) and `arguments` remain excluded. No new AST node types — uses `IsFunctionDeclaration` flag on `TGocciaVariableDeclaration`. [language.md § function Keyword](../language.md#function-keyword).
