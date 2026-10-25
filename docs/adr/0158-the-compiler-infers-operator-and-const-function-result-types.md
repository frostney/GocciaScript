# 0158 - The compiler infers operator results, proven consts and const function returns

**Date:** 2026-10-25
**Area:** `compiler`, `bytecode`
**Related:** [ADR 0001](0001-number-dual-representation.md), [Bytecode VM § Compiler Type Inference](../bytecode-vm.md#compiler-type-inference), [#1276](https://github.com/frostney/GocciaScript/issues/1276)

## Context

The compiler typed an expression only from literals, annotated or literal-typed
bindings and arithmetic between two typed operands. Anything else, such as
`a * 2` with an unknown `a`, the result of a unary operator or a call, was
untyped, and so was every binding initialized from it. The typed instructions
(`OP_ADD_FLOAT`, `OP_EQ_FLOAT` and the rest) do not check their operands, so a
type may only be claimed where the specification makes it certain.

Types also reach the bytecode: each function template records the type of every
typed local. A consumer that translates bytecode ahead of time reads them, and
untyped locals force it to keep every value tagged.

## Decision

**An operator's result type follows from ECMA-262, not from its operands' types
alone.** With one operand a proven Number, `-`, `*`, `/`, `%` and `**` yield a
Number: §13.15.3 `ApplyStringOrNumericBinaryOperator` applies ToNumeric to both
operands, and a Number with a BigInt throws. `+` is a Number only when both
operands are, and a String when either is a String. The bitwise operators
yield an integer Number unless both operands are BigInts (`>>>` always, since a
BigInt operand throws). Unary `+` is a Number, `-` and `~` of a proven Number
stay Numbers, `!` is a Boolean and `typeof` a String.

**A `const` is typed when `Goccia.Compiler.NumericBindings` proves its
initializer a Number.** The whole-body proof that already decides which `let`
bindings hold only Numbers also answers for a `const` of a plain name. A
destructuring pattern is not asked: extending the scan to nested destructuring
patterns made it exceed the time limit on `tests/language/nesting-limit/statements.js`.
Under `--strict-types` a `let` whose initializer is not a literal form is typed
only when the proof covers every assignment, and the type is not enforced.

**A call has the type of the `const` function it names, when every return has
one type.** The compiler records the join of a function's returns while it
compiles the body. A function that can reach its end, returns without a value,
is `async` or a generator, or returns different types has no return type. The
type is used for a plain, non-optional call by identifier, only when the name
resolves to a `const` binding that is not global-backed, no `with` object is in
scope, and no sloppy direct `eval` can shadow the name. A captured `let` that
only ever holds Numbers keeps its type inside closures.

**Not inferred:** `Math` calls and calls on any global, since a global binding
is writable; a return-type annotation, which is not enforced (#1276); a `var`.

## Consequences

- The VM runs the same code: for Numbers the typed and the generic arithmetic
  opcodes share one fast path, so no speed-up was measured on the benchmarks.
  The value is in the types the bytecode records, for ahead-of-time consumers.
- A local the compiler could not type before can now carry a type in its
  function template; no type that was already recorded changes.
- The integer type comes only from results that are an int32 or a uint32 by
  specification: the bitwise operators, `~` and `>>>`. No arithmetic result
  is an integer, `+` and `-` included, because a sum of integers can overflow
  to `Infinity`. The compiler used to give two integers an integer result for
  every arithmetic operator; that rule was dormant while nothing produced an
  integer, and typing bitwise results activated it, so it is removed.
- `tests/language/type-inference/` runs the cases that would expose a wrong
  type in the default and the `--strict-types` profiles.
