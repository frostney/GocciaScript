# Language Tables

*Quick-reference tables for GocciaScript's ECMAScript coverage and TC39 proposal support.*

## Executive Summary

- **ECMAScript table** — Feature-by-feature status from ES1 through ES2027, sorted chronologically
- **Web Platform APIs table** — WHATWG and W3C APIs supported beyond ECMA-262
- **TC39 proposals table** — Active proposals sorted by stage (highest first), with links to specs
- **Canonical source** — Detailed semantics, examples, and rationale live in [language.md](language.md)

## ECMAScript Feature Summary

| Feature | Spec | Status |
|---------|------|--------|
| `var` | ES1 | Opt-in (`--compat-var`) — use `let`/`const` by default |
| `function` keyword | ES1 | Opt-in (`--compat-function`) — use arrow functions or shorthand methods by default |
| Comma operator (`,`) | ES1 | Supported |
| `==` / `!=` (loose equality) | ES1 | Excluded — use `===` / `!==` |
| `eval()` | ES1 | Excluded |
| `arguments` object | ES1 | Excluded — use rest parameters |
| `for` / `while` / `do...while` | ES1 | Excluded — use `for...of` or array methods |
| `with` statement | ES1 | Excluded |
| ASI (automatic semicolon insertion) | ES1 | Opt-in (`--asi`) |
| Global `parseInt`, `parseFloat`, `isNaN`, `isFinite` | ES1 | Excluded — use `Number.*` |
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
| ES modules (`import` / `export`) | ES2015 | Supported (named only) |
| Default exports / imports | ES2015 | Excluded — use named exports |
| Generators (`function*`, `*method()`) | ES2015 | Supported — `function*` requires `--compat-function`; generator methods are default syntax |
| Nullish coalescing (`??`) | ES2020 | Supported |
| Optional chaining (`?.`) | ES2020 | Supported |
| `BigInt` | ES2020 | Supported |
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
| `Object.getOwnPropertyDescriptors` | ES2024 | Supported |
| Set methods (`union`, `intersection`, `difference`, etc.) | ES2025 | Supported |
| `Promise.try` | ES2025 | Supported |
| Iterator Helpers (`map`, `filter`, `take`, `drop`, etc.) | ES2025 | Supported |
| `Float16Array`, `Math.f16round` | ES2025 | Supported |
| Resizable `ArrayBuffer` (`resize`, `transfer`, `transferToFixedLength`) | ES2025 | Supported |
| `import.meta` | ES2020 | Supported |
| Dynamic `import()` | ES2020 | Supported |
| `Uint8Array` Base64/Hex (`fromBase64`, `fromHex`, `toBase64`, `toHex`) | ES2026 | Supported |
| `Error.isError` | ES2026 | Supported |
| `RegExp.escape` | ES2026 | Supported |
| `Array.fromAsync` | ES2026 | Supported |
| `Math.sumPrecise` | ES2026 | Supported |
| `Map.prototype.getOrInsert`, `getOrInsertComputed` | ES2026 | Supported |
| `WeakMap.prototype.getOrInsert`, `getOrInsertComputed` | ES2026 | Supported |
| `Iterator.concat` (Iterator Sequencing) | ES2026 | Supported |
| Explicit Resource Management (`using`, `await using`) | ES2026 | Supported |
| JSON.parse source text access (`JSON.rawJSON`, `JSON.isRawJSON`) | ES2026 | Supported |
| `Temporal` (dates, times, durations, time zones) | ES2027 | Supported |

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
| [Joint Iteration](https://github.com/tc39/proposal-joint-iteration) | 3 | Supported — `Iterator.zip` and `Iterator.zipKeyed` |
| [`Math.clamp`](https://github.com/tc39/proposal-math-clamp) | 2 | Supported |
| [Pattern Matching](https://tc39.es/proposal-pattern-matching/) | 1 | Supported — `value is Pattern`, `match`, filtered `for...of` / `for await...of`, and pattern catches |
| [Types as Comments](https://tc39.es/proposal-type-annotations/) | 1 | Supported — TypeScript-style annotations parsed, preserved on AST; runtime enforcement opt-in via `--strict-types` (works in both interpreter and bytecode) |
| [Enum Declarations](https://github.com/tc39/proposal-enum) | 0 | Supported — frozen, null-prototype enum objects with `Symbol.iterator` |

## Runtime Extensions

GocciaScript provides a `Goccia` global object with engine metadata and runtime APIs beyond the ECMAScript specification, including `spec` (implemented ES features by year) and `proposal` (implemented TC39 proposals by stage). See [Built-in Objects — Goccia object](built-ins.md#global-constants-functions-and-error-constructors-gocciabuiltinsglobalspas) for the full property reference.

## Related documents

- **Language** -- [language.md](language.md) -- Detailed semantics, examples, restrictions, and rationale
- **Built-ins** -- [built-ins.md](built-ins.md) -- API reference for all built-in types and functions
