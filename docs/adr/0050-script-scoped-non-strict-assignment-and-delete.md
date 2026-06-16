# Script-scoped non-strict assignment and delete

**Date:** 2026-05-15
**Area:** `engine`
**Pull Request:** [#645](https://github.com/frostney/GocciaScript/pull/645)

Tightened `--compat-non-strict-mode` to Script-source semantics and completed the assignment/delete behavior required by #586. Failed ordinary object/global writes now silently preserve the assigned expression value only in non-strict Script mode, identifier delete can remove configurable global object properties, modules ignore the compatibility flag for strict `this`/`arguments`/parser/compiler decisions, bytecode format v31 adds loose set/global-delete opcodes, and the test262 runner now passes the flag only to non-`onlyStrict` Script tests. [language.md](../language.md#non-strict-assignment-semantics). [language.md](../language.md#delete-semantics). [bytecode-vm.md](../bytecode-vm.md#compatibility-scope-helpers). [test262.md](../test262.md#strict-mode).
