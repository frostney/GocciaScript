# Language Tables

*Quick-reference tables for GocciaScript's recommended profile, web APIs, and TC39 proposal support.*

## Executive Summary

- **Profile table** — Implemented semantics, recommended defaults, exposure paths, and standards sources are separate fields
- **Generated evidence** — Core ECMAScript coverage is tracked by generated test262 reports rather than a hand-maintained feature inventory
- **Annex B policy** — Browser-only legacy Annex B semantics are not a pre-1.0 target; selected existing shims remain documented individually
- **Parser policy** — Disabled parser syntax is a `SyntaxError` by default; `--warning-unsupported-features` restores warning/no-op recovery without enabling compatibility semantics
- **Beyond core ECMAScript** — Separate tables cover Web Platform APIs and active TC39 proposals
- **Canonical source** — Detailed semantics, examples, and rationale live in [language.md](language.md)

## Recommended Profile Compatibility Paths

These forms are implemented. The table describes whether the recommended
profile exposes them, not whether the engine contains their semantics.

| Form | Implemented | Default profile | Enablement / exposure | Standards source |
|------|-------------|-----------------|-----------------------|------------------|
| `var` | Yes | Disabled | `--compat-var` | ECMA-262 core |
| traditional `function` syntax | Yes | Disabled | `--compat-function` | ECMA-262 core |
| `==` / `!=` | Yes | Disabled | `--compat-loose-equality` | ECMA-262 core |
| ASI | Yes | Disabled | `--compat-asi` | ECMA-262 core |
| labels | Yes | Disabled | `--compat-label` | ECMA-262 core |
| traditional `for(init; test; update)` | Yes | Disabled | `--compat-traditional-for-loop` | ECMA-262 core |
| `for...in` | Yes | Disabled | `--compat-for-in-loop` | ECMA-262 core |
| `while` / `do...while` | Yes | Disabled | `--compat-while-loops` | ECMA-262 core |
| `arguments` | Yes | Disabled | `--compat-arguments-object`; mapped semantics also require non-strict Script source | ECMA-262 core |
| non-strict Script semantics | Yes | Strict by default | `--compat-non-strict-mode`; modules remain strict | ECMA-262 core |
| `with` | Yes | Disabled | `--compat-non-strict-mode` in Script source | ECMA-262 core |
| `eval` | Yes | Not installed | Private `GocciaScriptLoaderBare --test262-host` only | ECMA-262 core |
| `Function()` | Yes | Disabled | `--unsafe-function-constructor` | ECMA-262 core |
| `ShadowRealm` | Yes | Not installed | `--unsafe-shadowrealm` | TC39 Stage 2.7 |

## Core ECMAScript Evidence

Core ECMAScript coverage is measured by generated test262 reports instead of a
hand-maintained list of implemented features. See the
[compatibility dashboard](https://www.gocciascript.dev/compatibility) for the
current results and [language.md](language.md) for detailed semantics and
profile policy.

Browser-only legacy Annex B semantics are not a pre-1.0 target. Existing
selected shims remain documented individually; see
[ADR 0085](adr/0085-defer-annex-b-before-1-0.md).

## Web Platform APIs

APIs from WHATWG and W3C specifications — not part of ECMA-262, but widely expected in JavaScript runtimes.

| API | Spec | Status |
|-----|------|--------|
| `console` (`log`, `warn`, `error`, `info`, `debug`, `dir`, `assert`, `count`, `time`, `table`, `trace`, …) | [WHATWG Console](https://console.spec.whatwg.org/) | Supported |
| `structuredClone` | [HTML §2.7.3](https://html.spec.whatwg.org/multipage/structured-data.html#dom-structuredclone) | Supported |
| `DOMException` | [Web IDL](https://webidl.spec.whatwg.org/#idl-DOMException) | Supported |
| `atob` / `btoa` | [HTML §8.3](https://html.spec.whatwg.org/multipage/webappapis.html#atob) | Supported |
| `queueMicrotask` | [HTML §8.4](https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#dom-queuemicrotask) | Supported |
| `URL`, `URL.parse`, `URL.canParse` | [WHATWG URL §4](https://url.spec.whatwg.org/#url-class) | Supported |
| `URLSearchParams` | [WHATWG URL §6](https://url.spec.whatwg.org/#urlsearchparams) | Supported |
| `TextEncoder` | [WHATWG Encoding §8.3](https://encoding.spec.whatwg.org/#textencoder) | Supported |
| `TextDecoder` | [WHATWG Encoding §8.2](https://encoding.spec.whatwg.org/#textdecoder) | Supported |
| `performance.now`, `timeOrigin` | [High Resolution Time](https://w3c.github.io/hr-time/#dom-performance-now) | Supported |
| `EventTarget`, `Event` | [WHATWG DOM §2](https://dom.spec.whatwg.org/#events) | Supported subset (single-target dispatch; no propagation) |
| `AbortController`, `AbortSignal` | [WHATWG DOM §3](https://dom.spec.whatwg.org/#aborting-ongoing-activities) | Supported subset (fetch-scoped; `abort` event and `onabort`; no `any`) |
| `fetch`, `Headers`, `Response` | [WHATWG Fetch](https://fetch.spec.whatwg.org/) | Supported (GET/HEAD only) |

## TC39 Proposals

| Proposal | Stage | Status |
|----------|-------|--------|
| [Joint Iteration](https://github.com/tc39/proposal-joint-iteration) | 4 (finished) | Supported — `Iterator.zip` and `Iterator.zipKeyed` |
| [Import Defer](https://tc39.es/proposal-defer-import-eval/) | 3 | Supported — static `import defer * as ns` and dynamic `import.defer()` create deferred namespace objects |
| [Iterator Includes](https://github.com/tc39/proposal-iterator-includes) | 3 | Supported — `Iterator.prototype.includes(searchElement, skippedElements?)` with SameValueZero comparison and iterator closing |
| [Decorators](https://github.com/tc39/proposal-decorators) | 2.7 | Supported — class, method, field, getter/setter, auto-accessor decorators with `addInitializer` |
| [Decorator Metadata](https://github.com/tc39/proposal-decorator-metadata) | 2.7 | Supported — `Symbol.metadata` for decorator-attached class metadata with inheritance |
| [Import Bytes](https://github.com/tc39/proposal-import-bytes) | 2.7 | Supported — `import x from "./f" with { type: "bytes" }` (static, dynamic, `import.defer`) yields a default-only `Uint8Array` over an immutable `ArrayBuffer` |
| [Immutable ArrayBuffers](https://github.com/tc39/proposal-immutable-arraybuffer) | 2.7 | Supported — `ArrayBuffer.prototype.transferToImmutable` plus the `immutable` getter; writes to immutable-backed views are rejected |
| [ShadowRealm](https://github.com/tc39/proposal-shadowrealm) | 2.7 | Opt-in (`--unsafe-shadowrealm`) — global `ShadowRealm` constructor with `evaluate`, `importValue`, and the wrapped-function callable boundary; off by default because it evaluates dynamic source and imports modules |
| [`Math.clamp`](https://github.com/tc39/proposal-math-clamp) | 2 | Supported |
| [Pattern Matching](https://tc39.es/proposal-pattern-matching/) | 1 | Supported — `value is Pattern`, `match`, filtered `for...of` / `for await...of`, and pattern catches |
| [Type Annotations](https://tc39.es/proposal-type-annotations/) | 1 | Supported — proposal-compatible types-as-comments semantics by default; GocciaScript's optional `--strict-types` extension adds runtime enforcement in interpreter and bytecode modes, not static structural checking. See [Type Annotations](type-annotations.md) |
| [Enum Declarations](https://github.com/tc39/proposal-enum) | 1 | Supported — frozen, null-prototype enum objects with `Symbol.iterator` |

## Runtime Extensions

GocciaScript provides a `Goccia` global object with engine metadata and runtime APIs beyond the ECMAScript specification, including `spec` (implemented ES features by year) and `proposal` (a selected inventory of implemented TC39 proposals by stage). See [Built-in Objects — Goccia object](built-ins.md#global-constants-functions-and-error-constructors-gocciabuiltinsglobalspas) for the full property reference.

## Related documents

- **Language** -- [language.md](language.md) -- Detailed semantics, examples, restrictions, and rationale
- **Built-ins** -- [built-ins.md](built-ins.md) -- API reference for all built-in types and functions
