# Opt-in var declarations

**Date:** 2026-04-20
**Area:** `runtime`
**Pull Request:** [#368](https://github.com/frostney/GocciaScript/pull/368)

Opt-in `var` declarations (`--compat-var`). Added `var` support behind a `--compat-var` CLI flag for ECMAScript compatibility when porting legacy code. Var bindings are stored in a separate `FVarBindings` map on function/module/global scopes (distinct from the lexical binding map used by `let`/`const`), matching the ES spec's separation between VariableEnvironment and LexicalEnvironment. This avoids forcing var semantics (function-scoped, redeclarable, no TDZ) through lexical machinery designed around block-scoping, TDZ, and no-redeclaration guarantees. The bytecode compiler uses a parallel `DeclareVarLocal` approach with depth-0 locals that survive `EndScope` unwinding. Naming follows spec terminology: `DefineLexicalBinding` / `DefineVariableBinding` / `GetBinding` / `AssignBinding`. [language.md § var Declarations](../language.md#var-declarations).
