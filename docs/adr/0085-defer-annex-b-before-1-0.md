# Defer broad Annex B support before 1.0

**Date:** 2026-07-01
**Area:** `language`, `test262`
**Issue:** [#853](https://github.com/frostney/GocciaScript/issues/853)
**Supersedes:** [ADR 0061](0061-annex-b-labelled-functions.md) as a general Annex B policy statement; ADR 0061 remains the historical record for the labelled-function implementation boundary.

GocciaScript will not pursue broad ECMA-262 Annex B support for this release or before 1.0. Annex B is a web-browser legacy compatibility annex, not part of the core ECMAScript language, and ECMA-262 discourages non-browser implementations from adding those features unless they must run the same legacy web code as browsers. That does not match the current product direction in [VISION.md](../../VISION.md): sandbox-first execution for agents, embeddable desktop runtimes, and no full browser host environment today.

The Annex B test262 baseline from #853 is retained as a measurement snapshot, not a release target: bytecode mode ran 1086 Annex B tests with 476 pass, 602 fail, 8 wrapper-infra failures, and 0 timeouts; interpreted mode ran the same 1086 tests with 495 pass, 583 fail, 8 wrapper-infra failures, and 0 timeouts. The existing passing surface mostly comes from features that were already implemented for broader ECMAScript compatibility or as targeted shims, such as `String.prototype.substr`, object legacy accessors, `Date.prototype.toGMTString`, and sloppy block-function behavior behind existing compatibility flags.

The decision is intentionally narrower than "remove all Annex B-shaped behavior". Existing implemented surfaces may remain when they already support current conformance work, avoid churn, or are exposed as documented shims. New work should not add Annex B parser, binding, object-model, RegExp legacy, or browser-host semantics just to improve the `annexB` category. A future Web API or browser-compatibility product direction may re-open Annex B as an explicit compatibility profile, because browser-style host compatibility is the point where Annex B becomes strategically coherent.

Consequences:

- `annexB` stays excluded from the default test262 category set.
- Annex B pass rate is informational only and should not be treated as a pre-1.0 release gate.
- Shim-level legacy names may still be implemented when they layer cleanly over existing modern primitives and are useful outside a browser-host profile.
- Native Annex B semantics that require parser, binding-instantiation, object-model, RegExp-internal, or host hooks need a separate post-1.0 design decision, preferably tied to a Web API/browser-compatibility profile.
