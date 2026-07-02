# GocciaScript

![GocciaScript logo](./logo.png)

A drop of JavaScript — A sandbox-first ECMAScript runtime implemented in FreePascal

It's based on the thought "What if we implement ECMAScript today, but make the recommended defaults modern, explicit, and sandbox-first". Error-prone, redundant, or high-risk legacy forms are off by default, while the engine and runtime can opt into the compatibility behavior needed for ECMAScript conformance and legacy code. See [Language](docs/language.md) for the full rationale.

## Features

GocciaScript implements modern ECMAScript: `let`/`const`, arrow functions, classes with private fields, `for...of`, async/await, ES modules, decorators, and TypeScript-style type annotations. Features that are error-prone, redundant, or security risks (`var`, `function` keyword, `==`/`!=`, `eval`, labels, traditional loops, `for...in`) are excluded by default; selected legacy forms are available through explicit conformance-focused compatibility flags.

Core ECMAScript compatibility is now a release-track objective. The default language remains curated and sandbox-first, but test262 runs on every PR and main commit so conformance work can be measured from generated reports instead of hand-maintained status claims. Annex B's browser-only legacy surface is not a pre-1.0 target; see [ADR 0085](docs/adr/0085-defer-annex-b-before-1-0.md).

See [Language](docs/language.md) for the complete specification of supported features, TC39 proposals, and exclusions.

### Built-in Objects

`console`, `Math`, `JSON`, `JSON5`, `TOML`, `YAML`, `JSONL`, `CSV`, `TSV`, `Object`, `Array`, `Number`, `String`, `RegExp`, `Symbol`, `Set`, `Map`, `WeakSet`, `WeakMap`, `Promise`, `Temporal`, `Iterator`, `Proxy`, `Reflect`, `ArrayBuffer`, `SharedArrayBuffer`, TypedArrays (`Int8Array`, `Uint8Array`, `Uint8ClampedArray`, `Int16Array`, `Uint16Array`, `Int32Array`, `Uint32Array`, `Float16Array`, `Float32Array`, `Float64Array`, `BigInt64Array`, `BigUint64Array`) with ArrayBuffer and SharedArrayBuffer backing, `fetch`, `Headers`, `Response` ([WHATWG Fetch](https://fetch.spec.whatwg.org/) — GET/HEAD only), `URL`, `URLSearchParams`, `TextEncoder`, `TextDecoder`, plus error constructors (`Error`, `TypeError`, `ReferenceError`, `RangeError`, `DOMException`).

See [Built-in Objects](docs/built-ins.md) for the complete API reference.

## Example

```javascript
class CoffeeShop {
  #name = "Goccia Coffee";
  #beans = ["Arabica", "Robusta", "Ethiopian"];
  #prices = { espresso: 2.5, latte: 4.0, cappuccino: 3.75 };

  getMenu() {
    return this.#beans.map((bean) => `${bean} blend`);
  }

  calculateTotal(order) {
    return order.reduce((total, item) => total + (this.#prices[item] ?? 0), 0);
  }

  get name() {
    return this.#name;
  }
}

const shop = new CoffeeShop();
const order = ["espresso", "latte"];
const total = shop.calculateTotal(order);

console.log(`Welcome to ${shop.name}!`);
console.log(`Your order total: $${total.toFixed(2)}`);
```

## Getting Started

### Prerequisites

- [FreePascal](https://www.freepascal.org/) compiler (`fpc`)
  - macOS: `brew install fpc`
  - Ubuntu/Debian: `sudo apt-get install fpc`
  - Windows: `choco install freepascal`

### Build

```bash
# Dev build of everything
./build.pas

# Production build
./build.pas --prod

# Build the script loader only
./build.pas loader
```

See [Build System](docs/build-system.md#build-commands) for build modes,
targets, clean builds, and troubleshooting.

### Run a Script

```bash
./build.pas loader && ./build/GocciaScriptLoader example.js
printf "const x = 2 + 2; x;" | ./build/GocciaScriptLoader --print
```

By default, both loaders are silent about the script's last evaluated value.
Pass `--print` to emit it; use `--output=json` for programmatic consumers. See
[Build System](docs/build-system.md#compile-and-run) for loader options,
bytecode mode, JSON output, sandbox execution, import maps, config files, and
resource limits.

### Run via Bytecode

GocciaScript includes bytecode execution, and `GocciaBundler` compiles source to
the public `.gbc` artifact.

```bash
./build/GocciaScriptLoader example.js --mode=bytecode
./build/GocciaBundler example.js
./build/GocciaScriptLoader example.gbc
```

See [Bytecode VM](docs/bytecode-vm.md) for the current bytecode executor architecture.

### Start the REPL

```bash
./build.pas repl && ./build/GocciaREPL
```

### Run Tests

GocciaScript has 11,000+ JavaScript unit tests covering language features, built-in objects, and edge cases.

```bash
./build.pas testrunner
./build/GocciaTestRunner tests
./build/GocciaTestRunner tests --mode=bytecode
```

See [Testing](docs/testing.md) for test organization and [Build System](docs/build-system.md#compile-and-test) for runner options.

### Run Benchmarks

```bash
./build.pas benchmarkrunner && ./build/GocciaBenchmarkRunner benchmarks
./build/GocciaBenchmarkRunner benchmarks/fibonacci.js
```

The benchmark runner auto-calibrates iterations per benchmark, reports ops/sec with variance (CV%) and engine-level timing breakdown (lex/parse/execute). Output formats: `console` (default), `text`, `csv`, `json`, `compact-json` (the same envelope as `json` without `build`, `memory`, `stdout`, or `stderr`). Calibration and measurement parameters are configurable via [environment variables](docs/benchmarks.md#configuring-benchmark-parameters).

## Quick Tour

GocciaScript looks like modern JavaScript — with a few intentional differences:

- **Arrow functions only** — `const greet = (name) => \`Hello, ${name}!\`;` (no `function` keyword)
- **No traditional loops** — `numbers.map((n) => n * 2)` or `for (const n of numbers) { ... }`
- **Classes** with private fields — `class Account { #balance = 0; ... }`
- **ES modules** — default, named, and namespace imports/exports are supported; project code prefers named exports for clarity.
- **Strict equality by default** — `===` and `!==` (`==`/`!=` require `--compat-loose-equality`)

The CLI tools share WHATWG-style import map support with `--import-map=<file.json>`, `--alias key=value`, and automatic `goccia.json` discovery for project-level module aliases.

Structured data files and text assets can also be imported directly:

```javascript
import { name, version } from "./package.json";
import { name as packageName } from "./config.toml";
import { name as appName } from "./config.yaml";
import { content, metadata } from "./README.md";
```

Runtime parsers are available for JSON5, TOML, YAML, JSONL, CSV, and TSV. See [Built-in Objects](docs/built-ins.md) and [Language](docs/language.md) for the full data format reference.

`TOML.parse(sourceText)` parses TOML 1.1.0 configuration data. `YAML.parse(sourceText)` handles common configuration files including block scalars, anchors/aliases, merge keys, and YAML 1.2 tag resolution. See [Language](docs/language.md#modules) and [Architecture Decision Records](docs/adr/) for the full conformance details.

JSONL parsing is also available via `JSONL.parse(text)` and `JSONL.parseChunk(text)`, and `.jsonl` files can be imported as structured-data modules.

**Async/await** with full Promise support, including top-level `await`:

```javascript
const fetchData = async () => {
  const result = await Promise.resolve({ status: "ok" });
  return result;
};

// Top-level await (ES2022+)
const data = await fetchData();
```

**Strict equality by default** — `===` and `!==`; `==` and `!=` are available only with `--compat-loose-equality`.

For a full guided walkthrough, see the [Tutorial](docs/tutorial.md). For the complete list of what's supported and excluded, see [Language](docs/language.md).

## Architecture

GocciaScript supports two execution modes that share the same source pipeline (preprocessors, lexer, parser, AST):

```mermaid
flowchart LR
    Source["Source Code"] --> Preprocessors["Preprocessors"] --> Lexer --> Parser --> AST
    AST --> Interpreter["Tree-Walk Interpreter"] --> Result1["Result"]
    AST --> Compiler["Bytecode Compiler"] --> VM["Goccia VM"] --> Result2["Result"]
```

Both execution modes share the same value types, built-ins, scope chain, and mark-and-sweep GC. The bytecode executor uses a Goccia-owned VM with tagged `TGocciaRegister` values (unboxed scalars) that fall back to `TGocciaValue` for heap objects, not a generic VM layer.

See [Architecture](docs/architecture.md) for pipelines and layers, [Interpreter](docs/interpreter.md) for tree-walk execution, [Bytecode VM](docs/bytecode-vm.md) for bytecode execution, [Core patterns](docs/core-patterns.md) for implementation patterns, and [GocciaScript Context](CONTEXT.md) for canonical terminology.

## Design Principles

- **Explicitness**: Modules, classes, methods, and properties use explicit, descriptive names even at the cost of verbosity. Shortcuts are avoided.
- **OOP over everything**: Rely on type safety of specialized classes rather than generic data structures.
- **Define vs Assign**: `Define` creates a new variable binding; `Assign` changes an existing one. These are distinct operations throughout the codebase (see [Core patterns](docs/core-patterns.md#define-vs-assign)).
- **Pure evaluation**: The evaluator is composed of pure functions with no side effects.
- **No global mutable state**: All runtime state flows through explicit parameters — the evaluation context, the scope chain, and value objects.
- **Virtual dispatch**: Property access (`GetProperty`/`SetProperty`), type discrimination (`IsPrimitive`/`IsCallable`), and scope chain resolution (`GetThisValue`/`GetOwningClass`/`GetSuperClass`) all use virtual methods, replacing type checks with single VMT calls.

See [Core patterns](docs/core-patterns.md) and [Interpreter](docs/interpreter.md) for the design rationale.

## Documentation

| Document | Description |
|----------|-------------|
| [Vision](VISION.md) | Why GocciaScript exists: sandboxed AI agent runtime and embeddable desktop platform |
| [Tutorial](docs/tutorial.md) | Your first GocciaScript program — a guided walkthrough for newcomers |
| [Language](docs/language.md) | ECMAScript support, recommended defaults, compatibility flags, and rationale |
| [Language Tables](docs/language-tables.md) | Quick-reference: ECMAScript feature matrix and TC39 proposal status |
| [Built-in Objects](docs/built-ins.md) | Available built-ins and API reference |
| [Temporal Built-ins](docs/built-ins-temporal.md) | Temporal API: dates, times, durations, time zones |
| [Binary Data Built-ins](docs/built-ins-binary-data.md) | ArrayBuffer, SharedArrayBuffer, TypedArray API |
| [Errors](docs/errors.md) | Error types, parser/runtime display, JSON output, `Error.cause`, `try`/`catch`/`finally` |
| [Architecture](docs/architecture.md) | Pipelines, main layers, design direction, duplication boundaries |
| [Interpreter](docs/interpreter.md) · [Bytecode VM](docs/bytecode-vm.md) | Tree-walk and bytecode execution modes |
| [Core patterns](docs/core-patterns.md) | Recurring implementation patterns |
| [GocciaScript Context](CONTEXT.md) | Canonical project terminology and glossary |
| [Value System](docs/value-system.md) | Type hierarchy, virtual property access, primitives, objects |
| [Garbage Collector](docs/garbage-collector.md) | Mark-and-sweep GC: architecture, contributor rules, design rationale |
| [Adding Built-in Types](docs/adding-built-in-types.md) | Step-by-step guide for adding new built-in types |
| [Embedding the Engine](docs/embedding.md) | Embedding GocciaScript in FreePascal applications |
| [Testing](docs/testing.md) | Test organization, running tests, coverage, CI |
| [Test Framework API](docs/testing-api.md) | Assertions, mocks, lifecycle hooks, async patterns |
| [Benchmarks](docs/benchmarks.md) | Benchmark runner, output formats, writing benchmarks |
| [Build System](docs/build-system.md) | Build commands, compiler configuration, CI/CD |
| [Profiling](docs/profiling.md) | Bytecode VM profiling: opcodes, functions, output formats |
| [Architecture Decision Records](docs/adr/) | Durable architectural decisions and trade-offs |
| [Contributing](CONTRIBUTING.md) | Single contribution standard: workflow, mandatory rules, testing, FreePascal style |
| [AGENTS.md](AGENTS.md) | Agent operating manual for coding assistants; [CONTRIBUTING.md](CONTRIBUTING.md) is the contributing guide for everyone |

## Contributing

**[CONTRIBUTING.md](CONTRIBUTING.md)** is the **contributing guide for all contributors** (humans and AI): workflow, mandatory rules, testing, FreePascal code style, `./format.pas`, editor setup, build/run quick reference, and the documentation index.

**[AGENTS.md](AGENTS.md)** (and **CLAUDE.md**, which points to it) is **only for AI assistants**—how to use the repo and defer to CONTRIBUTING. It is not a second contributing guide and should stay short.

## License

See [LICENSE](LICENSE) for details.
