# Compiler constant optimization

**Date:** 2026-05-03
**Area:** `bytecode`

Bytecode compilation now has an internal compiler-side constant optimizer for pure primitive folding, primitive `const` propagation, and dead branch/tail removal. The work deliberately stays format-neutral: no new VM opcodes or `.gbc` changes, and coverage mode preserves branch shape so missing branches remain reportable. [bytecode-vm.md § Compiler Optimizer](../bytecode-vm.md#compiler-optimizer).
