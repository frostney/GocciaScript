# Explicit function declaration nodes

**Date:** 2026-05-20
**Area:** `parser`

Function declarations use explicit statement nodes. Replaced the temporary `IsFunctionDeclaration` flag on `TGocciaVariableDeclaration` with `TGocciaFunctionDeclaration` and `TGocciaExportFunctionDeclaration`, while keeping function expressions on `TGocciaFunctionExpression`. This keeps declaration paths readable and maintainable, and gives parser, interpreter, bytecode, export, hoisting, and redeclaration logic exact AST semantics instead of inferring declaration intent from variable-declaration shape.
