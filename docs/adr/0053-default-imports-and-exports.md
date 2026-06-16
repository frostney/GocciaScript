# Default imports and exports

**Date:** 2026-05-18
**Area:** `engine`

Default imports and exports are supported across interpreter and bytecode script-module execution, including `import name from`, default-plus-named or namespace imports, `export default` expressions, and named default class/function declarations. "Named exports only" is no longer a language restriction; it remains the project convention for internal examples and new project code because named exports keep call sites explicit and refactors easier to review. [language.md § Modules](../language.md#modules). [language-tables.md](../language-tables.md).
