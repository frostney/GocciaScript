# Closed numeric scalar self-call frames

**Date:** 2026-07-21
**Area:** `bytecode runtime`

GocciaScript specializes direct self-calls only when the existing closed-world
numeric proof accepts the entire local arrow function and every external call.
For proven functions with one to three parameters, non-tail recursive call sites
emit `OP_CALL_SELF_NUM`. The instruction enters the same function template with
a compact scalar frame and a new register window while sharing the generic entry
frame's closure, lexical environment, local-cell and argument windows, realm,
`new.target`, and execution context. It still accounts for the ordinary stack
limit, deferred error-stack frames, function profiling, coverage, instruction
limits, timeouts, and GC-visible live registers. Tail calls continue through the
generic proper-tail-call path.

This is a proof-backed ABI, not speculative specialization. There is no guard or
deoptimization path: escapes, mixed or wrong-arity calls, optional/spread calls,
defaults, rest/destructuring, unsupported expression forms, and functions with
more than three parameters retain `OP_CALL`. Generic function entry is
unchanged. The compiler-only proof name is not serialized; the opcode is, so the
bytecode format advances from version 74 to 75.

On the retained untyped Fibonacci probe (25 inner iterations, nine interleaved
production samples), the scalar-frame build reduced median wall time from
241,359 us on `origin/main` to 72,211 us (70.1%), and from 228,870 us on the
numeric-superinstruction predecessor to 72,722 us (68.2%). Deterministic
one-iteration dispatch fell from 262,915 opcodes on `origin/main` to 164,408;
generic `OP_CALL` plus self `OP_GET_UPVALUE` dispatches fell from 43,785 to five
ordinary outer calls. The pinned AWFY Sieve guard remained within matched noise:
95,453 us versus 95,032 us on the predecessor (0.44% slower), and 95,254 us
versus 97,003 us on `origin/main` (1.8% faster). This narrowly satisfies the
end-to-end evidence requirement behind ADR 0089 without introducing a general
fixed-arity calling convention.
