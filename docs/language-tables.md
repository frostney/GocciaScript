# Language Tables

*Quick-reference tables for GocciaScript's ECMAScript coverage and TC39 proposal support.*

## Executive Summary

- **Profile table** — Implemented semantics, recommended defaults, exposure paths, and standards sources are separate fields
- **ECMAScript table** — Default-enabled feature status from ES1 through ES2027, sorted chronologically
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

## Implemented ECMAScript Surface

| Feature | Spec | Status |
|---------|------|--------|
| Comma operator (`,`) | ES1 | Supported |
| Annex B web-browser legacy features | ES Annex B | Deferred before 1.0; `annexB` test262 results are informational, not a release gate. Selected existing surfaces remain documented individually; broad support should be revisited only with a future Web API/browser-compatibility profile. See [ADR 0085](adr/0085-defer-annex-b-before-1-0.md) |
| Global `parseInt`, `parseFloat`, `isNaN`, `isFinite` | ES1 | Supported as legacy global shims installed through Goccia.shims; `parseInt`/`parseFloat` delegate to `Number.parseInt`/`Number.parseFloat`, while `isNaN`/`isFinite` keep standard global coercion behavior. Prefer `Number.*` in new code |
| `let` / `const` | ES2015 | Supported |
| Arrow functions | ES2015 | Supported |
| Classes (constructor, methods, static, getters/setters) | ES2015 | Supported |
| Template literals | ES2015 | Supported |
| Destructuring (array, object) | ES2015 | Supported |
| Spread / rest (`...`) | ES2015 | Supported |
| `for...of` | ES2015 | Supported |
| `Symbol` | ES2015 | Supported |
| `Promise` | ES2015 | Supported |
| `WeakMap`, `WeakSet` | ES2015 | Supported |
| `RegExp` (literals, flags `d`/`g`/`i`/`m`/`s`/`u`/`v`/`y`) | ES2015+ | Supported |
| ES modules (`import` / `export`) | ES2015 | Supported |
| Default exports / imports | ES2015 | Supported; project code convention prefers named exports for internal modules |
| Generators (`function*`, `*method()`) | ES2015 | Supported; `function*` requires `--compat-function`; implicit `arguments` objects require `--compat-arguments-object`; generator methods are default syntax |
| Nullish coalescing (`??`) | ES2020 | Supported |
| Optional chaining (`?.`) | ES2020 | Supported |
| `BigInt` | ES2020 | Supported |
| `WeakRef`, `FinalizationRegistry` | ES2021 | Supported |
| Logical assignment (`&&=`, `\|\|=`, `??=`) | ES2021 | Supported |
| Private fields and methods (`#field`) | ES2022 | Supported |
| Static class blocks | ES2022 | Supported |
| `Array.prototype.at` | ES2022 | Supported |
| `Object.hasOwn` | ES2022 | Supported |
| Top-level `await` | ES2022 | Supported |
| `Array.prototype.findLast`, `findLastIndex` | ES2023 | Supported |
| `Array.prototype.toReversed`, `toSorted`, `toSpliced`, `with` | ES2023 | Supported |
| `Object.groupBy`, `Map.groupBy` | ES2024 | Supported |
| `Promise.withResolvers` | ES2024 | Supported |
| `String.prototype.isWellFormed`, `toWellFormed` | ES2024 | Supported |
| `Object.getOwnPropertyDescriptors` | ES2017 | Supported |
| Set methods (`union`, `intersection`, `difference`, etc.) | ES2025 | Supported |
| `Promise.try` | ES2025 | Supported |
| Iterator Helpers (`map`, `filter`, `take`, `drop`, etc.) | ES2025 | Supported |
| `Float16Array`, `Math.f16round` | ES2025 | Supported |
| Resizable `ArrayBuffer` (`resize`, `transfer`, `transferToFixedLength`) | ES2024 | Supported |
| `import.meta` | ES2020 | Supported |
| `new.target` | ES2015 | Supported |
| Dynamic `import()` | ES2020 | Supported |
| `Uint8Array` Base64/Hex (`fromBase64`, `fromHex`, `toBase64`, `toHex`) | ES2026 | Supported |
| `Error.isError` | ES2026 | Supported |
| `RegExp.escape` | ES2026 | Supported |
| `Array.fromAsync` | ES2026 | Supported |
| `Math.sumPrecise` | ES2026 | Supported |
| `Map.prototype.getOrInsert`, `getOrInsertComputed` | ES2026 | Supported |
| `WeakMap.prototype.getOrInsert`, `getOrInsertComputed` | ES2026 | Supported |
| `Iterator.concat` (Iterator Sequencing) | ES2026 | Supported |
| Explicit Resource Management (`using`, `await using`) | ES2026 | Supported in interpreter and bytecode mode |
| JSON.parse source text access (`JSON.rawJSON`, `JSON.isRawJSON`) | ES2026 | Supported |
| `Temporal` (dates, times, durations, time zones) | ES2027 | Supported |
| `Intl` (ECMA-402) | ECMA-402 | Supported |

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
| `fetch`, `Headers`, `Response` | [WHATWG Fetch](https://fetch.spec.whatwg.org/) | Supported (GET/HEAD only) |

## TC39 Proposals

| Proposal | Stage | Status |
|----------|-------|--------|
| [Decorators](https://github.com/tc39/proposal-decorators) | 3 | Supported — class, method, field, getter/setter, auto-accessor decorators with `addInitializer` |
| [Decorator Metadata](https://github.com/tc39/proposal-decorator-metadata) | 3 | Supported — `Symbol.metadata` for decorator-attached class metadata with inheritance |
| [Import Defer](https://tc39.es/proposal-defer-import-eval/) | 3 | Supported — static `import defer * as ns` and dynamic `import.defer()` create deferred namespace objects |
| [Iterator Includes](https://github.com/tc39/proposal-iterator-includes) | 3 | Supported — `Iterator.prototype.includes(searchElement, skippedElements?)` with SameValueZero comparison and iterator closing |
| [Joint Iteration](https://github.com/tc39/proposal-joint-iteration) | 3 | Supported — `Iterator.zip` and `Iterator.zipKeyed` |
| [Import Bytes](https://github.com/tc39/proposal-import-bytes) | 2.7 | Supported — `import x from "./f" with { type: "bytes" }` (static, dynamic, `import.defer`) yields a default-only `Uint8Array` over an immutable `ArrayBuffer` |
| [Immutable ArrayBuffers](https://github.com/tc39/proposal-immutable-arraybuffer) | 2.7 | Supported — `ArrayBuffer.prototype.transferToImmutable` plus the `immutable` getter; writes to immutable-backed views are rejected |
| [ShadowRealm](https://github.com/tc39/proposal-shadowrealm) | 2.7 | Opt-in (`--unsafe-shadowrealm`) — global `ShadowRealm` constructor with `evaluate`, `importValue`, and the wrapped-function callable boundary; off by default because it evaluates dynamic source and imports modules |
| [`Math.clamp`](https://github.com/tc39/proposal-math-clamp) | 2 | Supported |
| [Pattern Matching](https://tc39.es/proposal-pattern-matching/) | 1 | Supported — `value is Pattern`, `match`, filtered `for...of` / `for await...of`, and pattern catches |
| [Type Annotations](https://tc39.es/proposal-type-annotations/) | 1 | Supported — proposal-compatible types-as-comments semantics by default; GocciaScript's optional `--strict-types` extension adds runtime enforcement in interpreter and bytecode modes, not static structural checking |
| [Enum Declarations](https://github.com/tc39/proposal-enum) | 0 | Supported — frozen, null-prototype enum objects with `Symbol.iterator` |

## Runtime Extensions

GocciaScript provides a `Goccia` global object with engine metadata and runtime APIs beyond the ECMAScript specification, including `spec` (implemented ES features by year) and `proposal` (a selected inventory of implemented TC39 proposals by stage). See [Built-in Objects — Goccia object](built-ins.md#global-constants-functions-and-error-constructors-gocciabuiltinsglobalspas) for the full property reference.

## Related documents

- **Language** -- [language.md](language.md) -- Detailed semantics, examples, restrictions, and rationale
- **Built-ins** -- [built-ins.md](built-ins.md) -- API reference for all built-in types and functions
