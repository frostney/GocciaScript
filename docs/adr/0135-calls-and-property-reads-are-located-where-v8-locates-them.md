# 0135 - A call and a failed property read are located where V8 locates them

**Date:** 2026-10-10
**Area:** `parser`, `bytecode compiler`, `diagnostics`
**Related:** [ADR 0014](0014-bytecode-and-interpreter-feature-parity.md), [ADR 0122](0122-unified-capability-model.md), [ADR 0131](0131-bytecode-frame-positions-at-capture.md)

## Context

A call expression's position is its call site. Both executors report it in a
stack frame that is making the call, in an audit event and in a
`PermissionDenied` error for that call. The parser put a call at its opening
parenthesis, and a dotted member expression at its `.`. V8 puts `inner(obj)` at
`inner` and `obj.x` at `x`, so a GocciaScript trace and Node's disagreed in the
column for nearly every frame
([#1494](https://github.com/frostney/GocciaScript/issues/1494)).

ECMA-262 says nothing about where a call or a property read is. The bytecode
compiler also located a failed read through the line map, where an
expression's entry precedes the code of its operands, so the load of `a.b.c`
was located at `a`.

## Decision

The parser gives each node the position V8 reports, so both executors and
every consumer of a call site agree:

- A call whose `(` follows an identifier token is located at that token: the
  callee of `f()`, `super` of `super()`, or the property name of `a.b()`.
  Any other call is located at its `(`. This is V8's rule, which keys on the
  token before the `(`: a reserved-word property (`p.catch()`), a private
  name (`this.#m()`), a parenthesized callee and an optional call (`f?.()`)
  all stay at the `(`.
- `new` is located at the `new` keyword, whatever its callee
  (`new ns.Widget()` had been located at `Widget`).
- A dotted member is located at its property name; a computed member stays at
  its `[`, and a private member at its `.`.

The bytecode compiler gives the instruction that loads a member's property the
member's position, and restores the line-map entry in effect before it for the
instructions that follow, so nothing else in the line map moves. The
interpreter stamps a failed read at the member rather than at the base of its
chain.

## Consequences

- Stack traces, the `-->` header and its caret, `--output=json`, audit events
  and `PermissionDenied` move from a call's `(` to its callee's name where V8
  puts it there.
- The instructions the compiler emits do not change; the line map gains up to
  two entries for each property read whose object needed code of its own. An
  entry at the same PC as the entry before it replaces that entry, which no
  lookup could reach, so the line maps end up smaller than before overall.
- Private-member reads and property writes keep their existing positions,
  which still differ from V8.
