# 0128 - The RegExp VM has two resource limits with their own messages, and a greedy single-character loop keeps one backtrack entry

**Date:** 2026-10-04
**Area:** `engine`

Refines [0044](0044-purpose-built-regexp-vm.md), which describes the step limit
as "configurable (default 10M)" and does not mention the backtrack stack.

## Context

`RunVM` in `Goccia.RegExp.VM.pas` stops a match attempt at either of two
fixed limits. Neither is configurable:

- **Step limit:** 100 VM steps per subject code unit, at least 10,000,000
  (`RegExpStepLimit`). It bounds the work of one attempt, such as catastrophic
  backtracking that the failure memo does not prune.
- **Backtrack-stack cap:** 10,000,000 entries (`DEFAULT_BACKTRACK_CAP`). It
  bounds the memory one attempt can hold for alternatives it has not tried yet.

Both threw `Error: Maximum regular expression backtrack stack size exceeded`,
so a step-limit failure could not be told apart from a stack failure. A greedy
loop pushed one entry per iteration, so `/a*b/` on more than 10,000,000 `a`s
followed by `b` hit the cap, where Node.js returns `true` (#1395).

## Decision

- Each limit throws its own `Error`: `Maximum regular expression step count
  exceeded` for the step limit, and `Maximum regular expression backtrack
  stack size exceeded` for the cap. ECMA-262 sets no resource limit on pattern
  matching; both are safety limits of this engine.
- A greedy loop whose body compiles to one character-matching instruction
  (`a*`, `.*`, `[a-z]+`, `[^x]{2,}`, `(?:a)*`) and that matches forward consumes
  its whole run in one step. When the rest of the pattern can fail, one
  backtrack entry stands for every shorter count. Popping it hands out one
  position at a time, longest count first, one code point back (a surrogate
  pair in unicode mode), so the order in which alternatives are tried does not
  change. Such a loop no longer reaches the cap on any subject length.
- A run that the rest of the pattern can backtrack into costs three steps per
  character, as iterating it did. A run whose tail can only accept stays free,
  as it was before.
- Other loops still keep one entry per iteration and can reach the cap. These
  include a body wider than one character (`(?:ab)*`, `(a|b)*`), a capturing
  body (`(a)*`), and any loop inside a lookbehind, which matches backward.

## Consequences

- `/a*b/`, `/.*b/` and `/[a-z]*b/` match on subjects of any length.
- Error messages tell users and tests which limit stopped the match.
- The single-entry run applies only when 32 or more code units remain at the
  loop. Shorter runs iterate. Both paths check the failure memo at the same
  states, so they find the same match.
