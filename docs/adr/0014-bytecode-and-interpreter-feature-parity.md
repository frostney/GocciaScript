# Bytecode and interpreter feature parity

**Date:** 2026-03-05
**Area:** `bytecode`
**Pull Request:** [#39](https://github.com/frostney/GocciaScript/pull/39)

Feature parity between bytecode and interpreter. The bytecode backend passes the full JavaScript test suite. [#39](https://github.com/frostney/GocciaScript/pull/39). [bytecode-vm.md](../bytecode-vm.md).

**Superseded by [ADR 0138](0138-bytecode-is-the-only-way-gocciascript-executes.md):** GocciaScript 0.15.0 removed the tree-walk
interpreter, so there is no second backend to hold at parity; bytecode is the
only way GocciaScript executes.
