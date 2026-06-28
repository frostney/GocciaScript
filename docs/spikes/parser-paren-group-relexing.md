# Parenthesized-group re-lexing

*Measurement record for [issue #808](https://github.com/frostney/GocciaScript/issues/808) / [ADR 0079](../adr/0079-keep-speculatively-scanned-tokens.md). Point-in-time snapshot — not maintained.*

## Executive Summary

- The arrow-vs-parenthesized probe (`IsArrowFunction`) and two sibling probes truncated their look-ahead tokens on rollback, so the real parse re-lexed every `(...)` group it scanned: O(n²) on nested groups, ~2× constant on arrow/callback-dense code.
- Keeping the probe's tokens unless one is goal-sensitive (`/` or template-tail `}`) eliminates the re-lexing for the common case.
- Nested-group **lexing** drops from O(n²) to linear: ≈287× faster at nesting depth 1600. Arrow-dense **lex+parse** is ≈2.1× faster on an 8k-declaration module — above the noise floor.
- The first attempt (goal-tagged re-derivation, as the issue proposed) was measured **broken**, not slow: it re-classified correct division `/` as regex and failed to parse the `Date` shim. The shipped design never re-derives an already-scanned token.
- Residual super-linear parse time on pathological nesting is the parser *re-reading* cached tokens (no character scanning); it is out of scope.

## Method

All numbers are `GocciaScriptLoader --output=json` phase timing (the same vehicle as [ADR 0037](../adr/0037-parser-hot-path-simplification.md)), median of 7 runs, macOS `darwin/aarch64`, FPC 3.2.2, development build. Two synthetic corpora:

- **nested** — one top-level expression `const _ = ((( … 0 … )));` at parenthesis depth *D*. Each `(` triggers a fresh `IsArrowFunction` probe over the whole inner span.
- **arrow-dense** — *N* declarations of `const fK = (a, b, c) => (a + b) * (c - a);`. Linear in *N*; every line has a parenthesized parameter list and parenthesized sub-expressions.

The benchmark runner is execution-only and its parse cost is a one-shot per-file metric; the dynamic `Function` constructor (which would let `run` re-parse) is disabled by default for sandbox safety, so the loader's parse-phase JSON is the measurement vehicle. `benchmarks/parser-arrow-groups.js` is committed as the matching arrow-execution benchmark.

## Results

Nested groups — lexing goes from O(n²) (≈4× per depth doubling) to linear (≈2×):

| depth | lex before | lex after | lex speed-up | parse before | parse after |
|------:|-----------:|----------:|-------------:|-------------:|------------:|
|   200 |    3.02 ms |   0.10 ms |         31×  |      1.67 ms |     0.66 ms |
|   400 |   11.83 ms |   0.18 ms |         65×  |      6.41 ms |     2.23 ms |
|   800 |   48.39 ms |   0.35 ms |        139×  |     25.73 ms |     8.16 ms |
|  1600 |  191.43 ms |   0.67 ms |        287×  |    100.51 ms |    31.24 ms |

Parse time stays super-linear after the fix because the parser still *re-reads* cached tokens once per enclosing probe — array reads, no lexing — so total front-end time at depth 1600 still falls from 292 ms to 32 ms (≈9×).

Arrow-dense — linear before and after, with a roughly halved lexing constant:

| decls | lex before | lex after | parse before | parse after | lex+parse before | lex+parse after |
|------:|-----------:|----------:|-------------:|------------:|-----------------:|----------------:|
|  2000 |   37.57 ms |  21.23 ms |     15.86 ms |    12.88 ms |          53.4 ms |         34.1 ms |
|  4000 |   86.48 ms |  43.06 ms |     52.18 ms |    25.68 ms |         138.7 ms |         68.7 ms |
|  8000 |  193.41 ms |  87.25 ms |    103.31 ms |    50.96 ms |         296.7 ms |        138.2 ms |

≈2.1× faster lex+parse at 8k declarations, consistent across sizes and well above the ±11–15% parser-benchmark noise floor.

## What did not work — goal-tagged re-derivation

The issue proposed tagging each cached token with its scan goal and re-deriving it whenever the parser later requested a different goal. Implemented, it failed at engine start-up: the `Date` shim's `Math.floor((year - 1969) / 4)` raised *“Unterminated regular expression literal.”* The parser peeks the operator `/` under its default `InputElementRegExp` goal and consumes it under `InputElementDiv`; re-deriving on that benign difference re-lexed a correct division `/` as a runaway regex. The lesson: an already-scanned token is authoritative and must never be re-classified — the only safe choice is whether a probe's tokens are kept at all, decided once from whether any of them is goal-sensitive.
