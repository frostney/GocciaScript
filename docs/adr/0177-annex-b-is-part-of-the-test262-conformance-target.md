# 0177 - Annex B is part of the test262 conformance target, and new Annex B surfaces sit behind a compatibility flag

**Date:** 2026-11-05
**Area:** `language`, `test262`, `embedding`
**Issue:** [#2063](https://github.com/frostney/GocciaScript/issues/2063)
**Supersedes:** [ADR 0085](0085-defer-annex-b-before-1-0.md)
**Related:** [ADR 0061](0061-annex-b-labelled-functions.md), [ADR 0046](0046-remove-compat-all-flag.md), [#853](https://github.com/frostney/GocciaScript/issues/853), [#2069](https://github.com/frostney/GocciaScript/issues/2069), [#1535](https://github.com/frostney/GocciaScript/issues/1535), [#1116](https://github.com/frostney/GocciaScript/issues/1116)

## Context

ADR 0085 deferred broad ECMA-262 Annex B support until after 1.0, kept
`annexB` out of the default test262 categories, and treated its pass rate as
informational. It argued from a sandbox-first, non-browser product direction.

The conformance target has since changed. It is now 100% of test262 in every
mode test262 requires, strict and non-strict, with Annex B and staging
included. 0.18.0 is the release that reaches 100%, and 1.0 requires it.
GocciaScript is primarily an engine for embedding, with sensible defaults that
a host can easily change. Annex B does not stop being legacy behaviour:
ECMA-262 (ES2026, Annex B introduction) still says implementations are
"discouraged from implementing these features unless the implementation is
part of a web browser or is required to run the same legacy ECMAScript code
that web browsers encounter". An embedder that runs legacy library code is
that second case, and test262 measures all of it. ADR 0085 would have a
contributor turn down work the release now needs.

## Decision

Annex B is part of GocciaScript's conformance target.

- `annexB` joins the default test262 categories and counts toward the
  published pass rate. Making the runner and CI run it by default, after the
  two runner defects below are fixed, is
  [#2069](https://github.com/frostney/GocciaScript/issues/2069).
- From 0.18.0 the `annexB` result is a release gate on the same terms as the
  other default categories.
- A new Annex B surface goes behind an `Engine.Compatibility` flag, off by
  default, with a matching `--compat-*` command-line flag and config key. The
  test262 runner's engine profile in `GocciaTest262Runner` turns it on, as it
  does `cfVar` and `cfNonStrictMode`. The default profile keeps its modern
  shape, and a host that needs the legacy surface turns on one flag. The 0.18.0
  Annex B work introduces the flag; this ADR does not add it.
- The flag follows the existing naming rule. Each compatibility flag is one
  `TGocciaCompatibility` member (`Goccia.SourcePipeline.pas`) and one
  `SOURCE_COMPATIBILITY_FLAGS` entry (`Goccia.CLI.Options.pas`), whose option
  name is `compat-` followed by a kebab-case name for the behaviour it enables
  (`cfLabel` is `--compat-label`, `cfForIn` is `--compat-for-in-loop`). The
  config key is the same name. No existing flag is named after a spec annex,
  so the spelling is left to the issue that adds the flag. Like every other
  compatibility flag, it is listed by name wherever it is needed: ADR 0046
  removed the `--compat-all` meta-flag, and nothing brings it back.
- Annex B surfaces that already ship stay as they are, always on or behind the
  flags they already have. They are not moved behind the new flag.

### Surfaces that already ship

Always on, in every profile:

- `String.prototype.substr` (B.2.2.1), a native method in
  `Goccia.Values.StringObjectValue.pas`.
- `String.prototype.trimLeft` and `String.prototype.trimRight` (B.2.2.15 and
  B.2.2.16), which are the `trimStart` and `trimEnd` function objects, in the
  same file.
- `Date.prototype.toGMTString` (B.2.3.3), in the Date shim in
  `Goccia.Shims.pas`, which returns `toUTCString()`.
- `RegExp.prototype.compile` (B.2.4.1), in `Goccia.Builtins.GlobalRegExp.pas`.
- The RegExp identity escapes of B.1.2 in a pattern without the `u` or `v`
  flag (`Goccia.RegExp.Compiler.pas`): `\a` matches `a`, `\p` matches `p`,
  `\k` matches `k` in a pattern without named groups, and an incomplete `\x`
  escape matches `x` and the characters after it. Other B.1.2 rules are not
  implemented yet: legacy octal escapes, `\8` and `\9` as identity escapes
  (both are back references today), an incomplete `\u` escape (a
  `SyntaxError`), and `\c` without a control letter (which matches `c` rather
  than `\c`).
- The [[IsHTMLDDA]] slot of B.3.6. IsLooselyEqual in `Goccia.Arithmetic.pas`
  checks it, and the only object that has it is the test262 host's
  `TGocciaTest262IsHTMLDDAValue` (`Goccia.Test262.Host.pas`), which also
  answers `typeof` and ToBoolean itself. A normal host has no such object, so
  the check changes nothing for it.

Also always on, and Annex B until ES2021: the legacy `Object.prototype`
accessor methods `__proto__`, `__defineGetter__`, `__defineSetter__`,
`__lookupGetter__` and `__lookupSetter__`, shims in `Goccia.Shims.pas`.
ES2022 moved them into the main body (ES2026 §20.1.3.8 and §20.1.3.9), and
their test262 tests are under `built-ins/Object/prototype`, not `annexB`.

Behind existing compatibility flags:

- Block-level function declarations (B.3.2) and function declarations as an
  `if` or `else` clause (B.3.3), in script source under `--compat-function`
  plus `--compat-non-strict-mode` (`Goccia.Compiler.BlockFunctions`).
- Labelled function declarations (B.3.1), in non-strict code under
  `--compat-label` plus `--compat-function`, as [ADR 0061](0061-annex-b-labelled-functions.md)
  records.
- A `var` redeclaring a simple catch parameter (B.3.4), wherever
  `--compat-var` allows `var`.

### Baseline

Measured at GocciaScript `7c6c8225` (origin/main) against the pinned test262
suite `91c70759363c46557b694792bf34a2551a7af5ba`
(`scripts/test262-suite-sha.txt`), with
`GocciaTest262Runner --categories=annexB` at the runner's defaults (4 jobs,
20 s per-test timeout, 2 GiB per-test heap): 1086 tests run, 787 pass, 291
fail, 8 wrapper-infra, 0 timeouts.

| Area | Pass | Fail | Wrapper-infra |
|------|-----:|-----:|--------------:|
| `built-ins/Array` | 1 | 0 | 0 |
| `built-ins/Date` | 2 | 22 | 0 |
| `built-ins/Function` | 1 | 5 | 0 |
| `built-ins/Object` | 0 | 1 | 0 |
| `built-ins/RegExp` | 27 | 35 | 0 |
| `built-ins/String` | 23 | 88 | 0 |
| `built-ins/TypedArrayConstructors` | 1 | 0 | 0 |
| `built-ins/escape` | 0 | 16 | 0 |
| `built-ins/unescape` | 0 | 19 | 0 |
| `language/comments` | 0 | 0 | 8 |
| `language/eval-code` | 397 | 72 | 0 |
| `language/expressions` | 5 | 21 | 0 |
| `language/function-code` | 158 | 1 | 0 |
| `language/global-code` | 153 | 0 | 0 |
| `language/literals` | 3 | 5 | 0 |
| `language/statements` | 16 | 6 | 0 |

Two of these results are runner defects, not engine gaps, and #2069 fixes
them: 20 failures are `SyntaxError: Identifier 'IsHTMLDDA' has already been
declared` from the runner's own harness, and the 8 wrapper-infra results are
the HTML-like comment tests. This table is a measurement snapshot; README and
website figures stay live-sourced from the published run (#1116).

## Consequences

- Annex B work is in scope from 0.18.0. A missing Annex B surface is a
  conformance gap like any other, for example `escape` and `unescape`
  ([#1535](https://github.com/frostney/GocciaScript/issues/1535)), which go
  behind the new flag.
- The default profile does not change. A host gets the new Annex B surfaces
  only by turning the flag on.
- Copy that says Annex B is "not a pre-1.0 target" or "deferred" is wrong and
  is updated with this ADR. Copy that describes the published run as
  excluding Annex B stays true until #2069 adds `annexB` to that run.
