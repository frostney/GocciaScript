# 0070 - Shared string-to-number conversion

## Status

Accepted.

## Context

ECMAScript `StringToNumber` (ECMA-262 §7.1.4.1.1) was implemented twice. Runtime
primitive coercion lived in `TGocciaStringLiteralValue.ToNumberLiteral`
(`Goccia.Values.Primitives`), and compile-time constant folding had its own
`StringToCompileTimeNumber` (`Goccia.Compiler.ConstantValue`). Each kept a
partial copy of the `StringNumericLiteral` grammar, so the interpreter and the
bytecode constant optimizer (ADR 0040) could fold the same string source to
different values. A `+Infinity` divergence had already been fixed in one path
but not the other, and both copies shared latent gaps: binary/octal prefixes
returned `NaN`, large hex wrapped through a fixed-width `StrToInt`, and
`TryStrToFloat` over-accepted inputs such as `"Inf"`.

The compiler must reuse the parser without instantiating a `TGocciaValue`, so
the shared implementation cannot return a runtime value object.

## Decision

Introduce `source/shared/NumericText.pas` with a single side-effect-free
`StringToNumber(const AText: string): Double`, and route both paths through it:
`ToNumberLiteral` wraps the result with `TGocciaNumberLiteralValue.Create`, and
`TryCompileTimeValueToNumber` calls it directly. The helper implements the
`StringNumericLiteral` grammar in one place — ECMAScript whitespace trimming,
empty `→ +0`, signed `Infinity`, `NonDecimalIntegerLiteral` (`0b`/`0o`/`0x`,
no sign, digits accumulated into a `Double` so large magnitudes round instead of
wrapping), a grammar-gated `StrDecimalLiteral` (overflow `→ ±Infinity`, signed
zero preserved), and `NaN` for everything else.

The helper lives in `source/shared/` rather than in `Goccia.Values.Primitives`
or `TextSemantics`:

- It returns a raw `Double` and depends only on text, so it stays free of the
  GocciaScript value types — the compiler reuses it with no allocation.
- A dedicated unit gives textual numeric conversion a clear home with room to
  absorb related parsing (`parseInt`, `parseFloat`) later, rather than growing
  `TextSemantics` (UTF/whitespace semantics) or `Goccia.Values.Primitives`
  (value classes) with an unrelated concern.

No bytecode format or opcode changes; the compiler optimizer design from
ADR 0040 is unchanged.

## Consequences

Runtime `Number(...)`/unary `+` and bytecode constant folding now share one
parser and cannot drift. Fixing the parser once also closed the shared
conformance gaps: binary/octal literals, full-magnitude hex, rejection of
`"Inf"` and bare punctuation, and sign rejection on non-decimal literals are now
correct in both execution modes. Future string↔number conversions should extend
`NumericText` instead of adding a second parser.
