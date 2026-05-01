# Parser Hot Dispatch Performance

Follow-up performance spike for reducing repeated token-helper work in `TGocciaParser`.

## Executive Summary

- The first parser spike found a measured binary-expression win, but a second pass showed the hotter duplicated pattern was repeated cursor and token-dispatch helper work.
- Caching the current token type inside `Advance`, `Check`, and `Match` improved post-binary parser medians by 3.7-26.8% on the earlier benchmark-runner synthetic map.
- Replacing remaining hot open-array checks and chained single-token `Match(...)` checks with direct token dispatch improved a same-corpus `GocciaScriptLoader --output=json` map by 35.1-49.1% versus clean `HEAD`.
- The implementation keeps recursive descent parsing and avoids parser unit splits. The changed surface is local to cursor helpers and hot expression dispatch.
- Standalone assignment-operator dispatch was mixed in isolation, so it is treated as part of the broader direct-dispatch cleanup rather than as an independent win.

## Context

The previous spike removed `ParseBinaryExpression(...)` from several precedence layers. That left the parser with many smaller duplicated costs:

- `Advance` called `IsAtEnd`, which called `Peek`, then `Previous`.
- `Check` called `IsAtEnd` and `Peek`.
- `Match(array)` called `Check` for each candidate, repeatedly rechecking EOF and reloading the current token.
- `Primary` and `Call` walked chains of single-token `Match(...)` calls on every expression.

All measurements were captured on 2026-04-29 on macOS `darwin/aarch64`, FPC 3.2.2, production builds.

## What Was Tried

### Cursor Helper Fast Path

`Advance`, `Check`, and both `Match` overloads now load `FTokens[FCurrent].TokenType` once for their decision. Successful `Match` calls advance `FCurrent` directly after the current token has already been proven not to be EOF.

This preserves the existing `Check(gttEOF) = False` behavior while removing nested helper calls from the hot path.

### Remaining Operator Dispatch

The second pass converted the remaining hot open-array expression checks to direct token-type checks:

- `Equality`
- `Comparison`
- `Unary`
- `Assignment`

`Equality` and `Comparison` still keep their custom behavior for loose-equality warnings, match-pattern `is`, and type annotations. Only their operator probes changed.

### Call/Member Loop Dispatch

`Call` now switches once on `Peek.TokenType` for call, member, computed member, private member, postfix increment/decrement, and tagged-template continuations. This replaces a repeated chain of negative `Match(...)` probes for every primary expression.

### Primary Expression Dispatch

`Primary` now switches once on the current token for literals, identifiers, `new`, `class`, `function`, grouping, arrays, and objects. The async-arrow, async-function, `match`, import, template, regex, and private-name branches still use the same parsing logic after dispatch.

### Unused Binary Helper Removal

After all measured precedence users moved to direct loops, `ParseBinaryExpression` and its `TParseFunction` callback type became unused and were removed.

## Cursor Helper Results

Ten production `GocciaBenchmarkRunner` runs per case. Baseline is the already-implemented binary-loop version from the first spike.

| Case | Binary-loop baseline | Cursor helper | Delta |
|---|---:|---:|---:|
| vars | 29.68ms | 22.64ms | 23.7% faster |
| binary | 29.50ms | 22.32ms | 24.3% faster |
| calls | 31.18ms | 23.30ms | 25.3% faster |
| objects | 33.71ms | 24.66ms | 26.8% faster |
| arrays | 34.57ms | 27.11ms | 21.6% faster |
| destructuring | 28.37ms | 22.40ms | 21.0% faster |
| classes | 19.09ms | 16.12ms | 15.6% faster |
| arrows | 25.15ms | 24.21ms | 3.7% faster |
| exports | 13.24ms | 10.51ms | 20.6% faster |

## Assignment Operator Probe

An assignment-only direct probe was measured after the cursor-helper change. Eight production `GocciaBenchmarkRunner` runs showed mixed movement:

| Case | Cursor helper | Assignment direct | Result |
|---|---:|---:|---:|
| vars | 22.64ms | 23.38ms | slower |
| binary | 22.32ms | 22.26ms | flat |
| calls | 23.30ms | 22.59ms | faster |
| objects | 24.66ms | 24.89ms | flat/slower |
| arrays | 27.11ms | 26.19ms | faster |
| destructuring | 22.40ms | 22.46ms | flat |
| classes | 16.12ms | 18.29ms | slower |
| arrows | 24.21ms | 21.47ms | faster |
| exports | 10.51ms | 11.72ms | slower |

This was not strong enough to claim as a standalone optimization. It stayed in the final implementation because it removes the largest remaining assignment open-array match and is covered by the broader final dispatch map.

## Final Dispatch Map

Twenty production runs per case. Values are median parser phase duration from `GocciaScriptLoader --output=json --asi`.

The baseline loader was built from a temporary detached clean `HEAD` worktree. The final loader was built from this parser branch. Both loaders parsed the same generated files, with runs alternated to reduce ordering bias.

| Case | Clean `HEAD` | Final dispatch | Delta |
|---|---:|---:|---:|
| vars | 16.23ms | 8.25ms | 49.1% faster |
| binary | 64.38ms | 35.85ms | 44.3% faster |
| calls | 38.17ms | 22.95ms | 39.9% faster |
| objects | 53.46ms | 31.62ms | 40.9% faster |
| arrays | 47.36ms | 27.32ms | 42.3% faster |
| destructuring | 72.99ms | 44.72ms | 38.7% faster |
| classes | 59.39ms | 38.54ms | 35.1% faster |
| arrows | 45.95ms | 29.38ms | 36.1% faster |
| exports | 7.06ms | 3.87ms | 45.3% faster |

The loader-based map is not directly comparable to the benchmark-runner map because it avoids benchmark execution and uses a separate generated corpus. It is useful for same-corpus before/after parser timing.

## What Was Not Tried

### Parser Unit Splits

Large structural parser splits were not attempted. They may help maintainability later, but they are not a parser-speed strategy by themselves and would require new unit ownership/API decisions. The measured hot spots were local enough to fix in place.

### Token-Type Side Array

A side array of token types could avoid dereferencing token objects during parser probes. This was not tried because it adds parser construction cost and extra memory traffic, while the helper and dispatch changes already removed the repeated nested calls that showed up in timing.

### AST Allocation Pooling

Pooling AST nodes was not tried. It crosses parser, AST ownership, and garbage-collection assumptions, and it would be harder to isolate as a parser-only improvement.

### Explicit Inline Annotations

Explicit compiler `inline` recommendations were not retried. The earlier spike found that adding `inline` to `Advance` regressed production medians, and production builds already enable automatic inlining through shared compiler settings.

### Pratt/Table-Driven Rewrite

A table-driven or Pratt parser rewrite was not attempted. It could simplify precedence metadata, but it would be a semantic rewrite rather than a targeted hot-path cleanup.

## Decision

Keep the recursive descent parser, but make its hot token paths direct:

- cache the current token type inside cursor helpers;
- use direct operator loops for hot expression levels;
- dispatch `Call` continuations with one token switch;
- dispatch `Primary` with one token switch;
- remove the unused generic binary helper.

This gives broad measured parser improvement without introducing new parser units or changing the public parser API.
