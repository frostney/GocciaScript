# Language Tables

*Quick-reference tables for GocciaScript's ECMAScript coverage and TC39 proposal support.*

## Executive Summary

- **ECMAScript table** — Feature-by-feature status from ES1 through ES2026, sorted chronologically
- **TC39 proposals table** — Active proposals sorted by stage (highest first), with links to specs
- **Canonical source** — Detailed semantics, examples, and rationale live in [language.md](language.md)

## ECMAScript Feature Summary

| Feature | Spec | Status |
|---------|------|--------|
| `var` | ES1 | Excluded — use `let`/`const` |
| `function` keyword | ES1 | Excluded — use arrow functions or shorthand methods |
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
| `RegExp` (literals, flags `d`/`g`/`i`/`m`/`s`/`u`/`v`/`y`) | ES2015+ | Supported |
| ES modules (`import` / `export`) | ES2015 | Supported (named only) |
| Default exports / imports | ES2015 | Excluded — use named exports |
| Generators (`function*`) | ES2015 | Not supported |
| Nullish coalescing (`??`) | ES2020 | Supported |
| Optional chaining (`?.`) | ES2020 | Supported |
| Logical assignment (`&&=`, `\|\|=`, `??=`) | ES2021 | Supported |
| Private fields and methods (`#field`) | ES2022 | Supported |
| Static class blocks | ES2022 | Supported |
| `Array.prototype.at` | ES2022 | Supported |
| `Object.hasOwn` | ES2022 | Supported |
| `structuredClone` | ES2022 | Supported |
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
| `Uint8Array` Base64/Hex (`fromBase64`, `fromHex`, `toBase64`, `toHex`) | ES2025 | Supported |
| `atob` / `btoa` | ES2025 | Supported |
| `import.meta` | ES2026 | Supported |
| Dynamic `import()` | ES2026 | Supported |

## TC39 Proposals

| Proposal | Stage | Status |
|----------|-------|--------|
| [`Error.isError`](https://github.com/tc39/proposal-is-error) | 4 | Supported |
| [Decorators](https://github.com/tc39/proposal-decorators) | 3 | Supported — class, method, field, getter/setter, auto-accessor decorators with `addInitializer` |
| [Decorator Metadata](https://github.com/tc39/proposal-decorator-metadata) | 3 | Supported — `Symbol.metadata` for decorator-attached class metadata with inheritance |
| [Temporal](https://tc39.es/proposal-temporal/) | 3 | Supported — `Temporal.PlainDate`, `Temporal.Duration`, `Temporal.Instant`, etc. |
| [`Math.clamp`](https://github.com/tc39/proposal-math-clamp) | 3 | Supported |
| [`Math.sumPrecise`](https://github.com/tc39/proposal-math-sum) | 3 | Supported |
| [`Map.prototype.getOrInsert`](https://github.com/tc39/proposal-upsert) | 3 | Supported — `getOrInsert` and `getOrInsertComputed` |
| [`RegExp.escape`](https://github.com/tc39/proposal-regex-escaping) | 3 | Supported |
| [Explicit Resource Management](https://github.com/tc39/proposal-explicit-resource-management) | 3 | Supported — `using` and `await using` declarations with `Symbol.dispose` / `Symbol.asyncDispose` |
| [Types as Comments](https://tc39.es/proposal-type-annotations/) | 1 | Supported — TypeScript-style annotations parsed, preserved on AST, enforced in bytecode mode |
| [Enum Declarations](https://github.com/tc39/proposal-enum) | 0 | Supported — frozen, null-prototype enum objects with `Symbol.iterator` |

## Runtime Extensions

GocciaScript provides a `Goccia` global object with engine metadata and runtime APIs beyond the ECMAScript specification, including `spec` (implemented ES features by year) and `proposal` (implemented TC39 proposals by stage). See [Built-in Objects — Goccia object](built-ins.md#global-constants-functions-and-error-constructors-gocciabuiltinsglobalspas) for the full property reference.

## Related documents

- **Language** -- [language.md](language.md) -- Detailed semantics, examples, restrictions, and rationale
- **Built-ins** -- [built-ins.md](built-ins.md) -- API reference for all built-in types and functions
