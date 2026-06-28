# Keep speculatively-scanned tokens across parenthesized-group probes

**Date:** 2026-06-28
**Area:** `parser`
**Issue:** [#808](https://github.com/frostney/GocciaScript/issues/808)

The parser pulls tokens from the lexer on demand: [`EnsureToken`](../../source/units/Goccia.Parser.pas) calls `TGocciaLexer.ScanNextToken` only when the requested index is past the end of the shared `FTokens` list, and the lexer appends each scanned token there ([Architecture — parser-owned lexical goals](../architecture.md)). Three sites speculatively scan a parenthesized `(...)` group before committing to an interpretation, then roll back: `IsArrowFunction` (arrow-vs-parenthesized disambiguation), `IsMatchExpressionAhead`, and `LooksLikeTraditionalForHeader`. Rollback went through `TGocciaLexer.RestoreCheckpoint`, which **truncated** `FTokens` back to the pre-probe length. So when the real parse advanced over the same span, `EnsureToken` found the list short again and re-tokenized the identical characters. Every parenthesized expression and parenthesized-parameter arrow lexed its span twice; deeply nested groups re-lexed inner spans once per enclosing level — O(n²) tokenization on pathological nesting, and a ~2× constant factor on ordinary arrow/callback-dense code (lexing is the dominant front-end phase here, ~2× parse time).

## Decision

A speculative probe **keeps** the tokens it scanned for the real parse to reuse, instead of truncating them — **unless** the probe scanned a *goal-sensitive* token, in which case it falls back to the previous truncate-and-re-lex behaviour for that group.

A token is goal-sensitive when the lexer can produce a different token at the same source position depending on the [`TGocciaLexicalGoal`](../../source/units/Goccia.Lexer.pas). Only two lexer branches are goal-dependent, so only their outputs qualify:

- `/` — a regex literal (`gttRegex`) when the goal allows regex, otherwise a division operator (`gttSlash` / `gttSlashAssign`).
- a template-tail `}` — `gttTemplateMiddle` / `gttTemplateTail` when the goal requires template continuation, otherwise an ordinary `gttRightBrace`.

Every other token type is classified identically under any goal, so a token a probe scanned (under its own goal) is byte-for-byte what the real parse would have produced — reusing it is behaviour-preserving. A probe that scanned a `/` or template-tail `}` may have classified it under the wrong goal (a probe scans the whole group under one goal, e.g. `IsArrowFunction` under `InputElementDiv`), so those groups are dropped and re-lexed under the real parse's per-token goals, exactly as before.

The lexer exposes `HasGoalSensitiveTokenSince(ACount)` (an O(1)-per-token scan of the newly-appended range). Each probe's `finally` keeps its look-ahead tokens unless that returns true (or, for the two retry sites, a lexer error means the group is being retried under the other goal). No per-token goal metadata is stored and no already-scanned token is ever re-classified.

## Rejected alternative — goal-tagged re-derivation

The issue proposed tagging every cached token with the goal it was scanned under and re-deriving it on demand whenever the parser later requested a different goal. This was implemented and rejected: it breaks the engine's standing invariant that **an already-scanned token is authoritative and never re-classified**. The parser routinely *peeks* a token under its default `InputElementRegExp` goal and then *consumes* it under `InputElementDiv` (e.g. an operator-position `/`); re-deriving on that benign goal difference turned a correct division `/` into an unterminated regex literal and failed to parse the built-in `Date` shim at engine start-up. The accepted design never re-derives — it only chooses, once, whether a probe's tokens are safe to keep.

## Consequences

- Re-lexing of parenthesized groups is eliminated for the common case (no regex/template inside the group). Measured with `GocciaScriptLoader --output=json` parse-phase timing: nested-group lexing drops from O(n²) to linear (≈287× faster lexing at depth 1600), and arrow/callback-dense lexing roughly halves (lex+parse ≈2.1× faster on an 8k-declaration module) — well above the parser benchmark noise floor. Groups containing a `/` or template literal re-lex as before (rare). See [the spike](../spikes/parser-paren-group-relexing.md).
- The residual super-linear cost on pathological nesting is the parser *re-reading* cached tokens across nested probes (cheap array reads, no character scanning or token allocation); eliminating it would need parser-level memoization of probe results and is out of scope here.
- No conformance change: the full JavaScript suite passes in both modes, and test262 `language` regexp/division/template-literal/arrow-function/asi areas pass 100% on the bytecode VM — expected, since classification is preserved exactly.
- `IsArrowFunction` no longer rolls back the lexer on the no-error path; it scans under `InputElementDiv` only and never catches lexer errors, so a lex error there remains a genuine, fatal syntax error.
