# 0179 - eval is an engine-only, opt-in installer, `EnableEval`, with no CLI flag and no config key

**Date:** 2026-11-05
**Area:** `engine`, `embedding`, `test262`
**Related:** [#2065](https://github.com/frostney/GocciaScript/issues/2065), [PR #2107](https://github.com/frostney/GocciaScript/pull/2107), [ADR 0073](0073-opt-in-shadowrealm.md), [ADR 0075](0075-shadowrealm-conformance.md), [ADR 0134](0134-direct-eval-compiles-as-a-closure-of-its-call-site.md), [VISION.md](../../VISION.md)

## Context

GocciaScript keeps dynamic source evaluation out of the default runtime. The
`Function` constructor is gated by `Engine.FunctionConstructor.Enabled`
(`--unsafe-function-constructor`) and `ShadowRealm` by `EnableShadowRealm`
(`--unsafe-shadowrealm`). `eval` had no supported switch: the default runtime
has no `eval` global, and the only code that installed one was a pair of private
copies, one in the test262 host and one for ShadowRealm child realms
([ADR 0075](0075-shadowrealm-conformance.md) gave a child its own `eval` when
the creating realm had an own `eval` property). An embedder who wanted `eval`
had to write a third copy, and the ShadowRealm rule keyed on whatever the
creator's script had left in its global.

GocciaScript is primarily an engine for embedding, with sensible defaults that
are easy to customise. The knobs are the existing ones plus this one; there are
no presets.

## Decision

### An installer in the `EnableShadowRealm` style

`EnableEval(Engine)` in `Goccia.Builtins.GlobalEval` installs `globalThis.eval`
as the realm's `%eval%`. `IsEvalEnabled(Engine)` reports whether it has run. It
is off by default, idempotent, and an engine cannot turn it off again.

It is an installer, like `EnableShadowRealm`, and not an `Enabled` toggle like
`FunctionConstructor.Enabled`. The `Function` constructor is an intrinsic that
is always present and throws while disabled. `ShadowRealm` and `eval` are
globals that must stay absent until a host installs them, so that
`typeof eval` and `"eval" in globalThis` show exactly what the host chose. A
toggle would leave a present-but-throwing `eval`, which is observable and which
a script can mistake for support. The scope comment on #2065 describes the
option as "in the `FunctionConstructor.Enabled` style", meaning opt-in, off by
default and engine API only. The shape is the installer's.

The installed function is writable, configurable, not enumerable, not a
constructor, and has no `prototype` (ES2026 §19.2.1). Recording it as the
realm's `%eval%` is what makes a call written `eval(...)` a direct eval
(§13.3.6.1) at the VM call site, by the identity check from
[ADR 0134](0134-direct-eval-compiles-as-a-closure-of-its-call-site.md); the
realm keeps the function, so that check still holds after a script replaces or
deletes the global. Any other call is an indirect eval (§19.2.1) and runs in
the global scope.

### No CLI flag and no config key

The option is exposed through the engine API only. The shipped runners get no
`--unsafe-eval` switch and no config key, so their runtime keeps no `eval`, with
or without `--unsafe-function-constructor` and `--unsafe-shadowrealm`. Johannes
confirmed this on 2026-11-05 and recorded it as a scope comment on #2065; the
existing `unsafe-` switches keep their names. No shipped runner needs a CLI
surface for it. If one is ever found to, that is a new decision and a new ADR,
not an addition to this option. `scripts/test-cli-apps.ts` checks that eval is
absent in host mode and in sandbox mode, including in a ShadowRealm child.

### The `eval.call` audit event

Every call of `%eval%`, direct or indirect, reports an `eval.call`
[capability audit event](../capability-audit.md) with decision `allow`, subject
`eval`, and a reason that says whether the call was direct or indirect. An
indirect call reports from the installed function; a direct call never reaches
that function, so the VM reports it at the call site, located at the `eval`
call expression, as a native callee's event is located at its call. A host that
runs untrusted code leaves `eval` off, as it does `Function` and `ShadowRealm`.

### test262 and ShadowRealm children use the option

The test262 host installs `eval` through `EnableEval`, as an embedder would,
and its private copy is gone. A `ShadowRealm` child is no longer given an
`eval` by inspecting the creator's global. **A child realm gets its own `eval`
only when the creating engine has the option on (`IsEvalEnabled`).** This
changes [ADR 0075](0075-shadowrealm-conformance.md)'s rule in two ways. A
creator whose script defined its own global `eval` no longer gives the child
one, and a creator that enabled `eval` and then deleted or replaced its global
still does. The child's `eval` evaluates in the child realm, so isolation holds
as before, and reports to the audit sink the child inherited. The test262
host enables the option, so a child realm there still gets its `eval`.

### Rejected

- **`Engine.EvalEnabled := True`.** It is the `Function` constructor's shape,
  and gives a present-but-throwing `eval` while it is off. See above.
- **A `--unsafe-eval` flag and a config key.** The decision on #2065 is that
  `eval` is exposed through the engine API only, so a config file cannot turn on
  dynamic code evaluation.
- **Keying the ShadowRealm child on the creator's global `eval`.** A script can
  change that, so it says nothing about what the host chose.
- **A `Compatibility` flag.** `Engine.Compatibility` selects language-profile
  forms, and `eval` is dynamic code generation, which sits with the
  `unsafe-` capability switches.

## Consequences

- An embedder opts into `eval` with one call, and the audit stream shows every
  use.
- `docs/embedding.md` lists all four knobs in one place ("Changing the
  Defaults").
- The test262 runner and the ShadowRealm child share the embedder's code path
  instead of two conformance-only copies.
- A ShadowRealm child of an engine without the option has no `eval`, even when
  the creator's script defined a global named `eval`.
- Adding a CLI surface later is an additive change to the runners; the engine
  option does not change.
