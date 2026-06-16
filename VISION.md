# Vision

*Why GocciaScript exists and what it aims to be.*

## Executive Summary

- **Primary goal** — A sandboxed JavaScript runtime for AI agents: reduced attack surface with no ambient file system, network, or process access by default
- **Secondary goal** — A modern ECMAScript platform embeddable in desktop applications, enabling portable code that runs both on the desktop (via FreePascal) and on the web (via standard browsers)
- **Design philosophy** — Default to the parts of JavaScript that lead to clear and predictable code; keep legacy and high-risk behavior opt-in
- **Release-track objective** — Reach full ECMAScript conformance as closely as possible for the next major version, measured by the generated test262 reports
- **Not a goal** — Node.js host compatibility, a full browser host environment, or unbounded server workloads

## Primary: Sandboxed Runtime for AI Agents

AI agents need to execute generated code in a constrained environment. GocciaScript provides a JavaScript execution environment that reduces the attack surface by default:

- **No ambient authority** — Scripts cannot access the file system, network, operating system, or native FFI unless the host explicitly grants capability through embedding APIs or unsafe runtime opt-ins such as `--unsafe-ffi`
- **No script-visible file system by default** — Scripts do not get a general read/write file-system API. Hosts can provide file-backed module/content loading or custom module content providers; a bounded script-visible virtual file system remains future work
- **Read-only network access** — A WHATWG `fetch` (GET/HEAD only) gated by an explicit host allowlist (`--allowed-host`); without an allowlist any call to `fetch` throws `TypeError`, so scripts cannot retrieve data or reach the network unless the host opts in
- **Deterministic core execution** — For core-language execution with the same source, config, and host-provided runtime surface, behavior is repeatable and intrinsic state does not leak between engine instances; hosts that install time, network, or other external runtime extensions own those sources of nondeterminism
- **Timeout enforcement** — The `--timeout` option and embedding API allow hosts to kill runaway scripts
- **Controlled runtime surface** — The host chooses which runtime extensions or profiles to install (see [Embedding](docs/embedding.md))
- **No dynamic code generation by default** — `eval()` is excluded from normal runtimes and the `Function()` constructor is excluded unless the host opts in via `--unsafe-function-constructor`; the private test262 host exposes a conformance-only eval hook
- **Modern function syntax by default** — Arrow functions, methods, accessors, and class methods are the recommended forms; the `function` keyword (declarations and expressions) is available only through `--compat-function` for ECMAScript compatibility

The sandbox is a *reduced attack surface*, not a formally verified security boundary. The engine has not been audited by a third party. The sandboxing relies on the host not exposing dangerous APIs, on `eval` being absent from normal runtimes, on the test262 eval hook remaining private to `GocciaScriptLoaderBare --test262-host`, and on the host not opting in to `--unsafe-function-constructor` (which would re-enable dynamic code generation via `Function()`).

## Secondary: Embeddable Desktop Platform

Desktop applications built with FreePascal (Lazarus, command-line tools, game engines) can embed GocciaScript to add scripting capabilities:

- **Portable code** — Scripts written with GocciaScript's recommended defaults use modern standard JavaScript forms and can be shared with browser code. Hosts can opt into compatibility behavior when they need older ECMAScript semantics.
- **Single-binary deployment** — GocciaScript compiles into the host application with no external runtime dependencies
- **Custom globals** — The host injects application-specific APIs through `DefineLexicalBinding` and custom built-in types (see [Embedding](docs/embedding.md) and [Adding Built-in Types](docs/adding-built-in-types.md))
- **Cross-platform** — Runs on macOS, Linux, Windows, and FreeBSD via FreePascal's cross-compilation

## What GocciaScript is Not

- **Not Node.js host compatible** — GocciaScript does not aim to support CommonJS, `node:` built-ins, npm-style package resolution, or Node's host environment. Some Node-authored scripts may run when they use standard ECMAScript or supported runtime globals, but Node.js compatibility is not a project goal.
- **Not a browser host environment today** — GocciaScript implements selected web-standard APIs where they fit the sandboxed runtime and embeddable platform goals, but it does not currently provide a DOM, Web Workers, storage APIs, browser event loop, or full browser host environment. Broader WinterTC and WHATWG runtime compatibility remains an open product direction.
- **Not a single fixed runtime surface** — The recommended defaults are sandbox-first and modern, but the engine and runtime are customizable. Compatibility flags exist primarily for ECMAScript conformance and legacy semantic requirements; userland code should usually prefer the default forms instead of enabling flags preemptively. `var`, the `function` keyword, `arguments`, `with`/`delete`/`this` non-strict semantics, loose equality, labels, traditional loops, `for...in`, and `while`/`do...while` loops remain opt-in via targeted `--compat-*` flags. Runtime enforcement of type annotations is also opt-in (`--strict-types`). See [Language](docs/language.md) for the full list.
- **Not a formally verified sandbox** — The sandbox reduces attack surface but has not been independently audited
- **Not performance-competitive with V8/SpiderMonkey** — GocciaScript prioritizes correctness, embeddability, and reduced attack surface over raw throughput

## Related documents

- [Language](docs/language.md) — ECMAScript support, recommended defaults, compatibility flags, and shims
- [Embedding](docs/embedding.md) — How to embed GocciaScript in a FreePascal application
- [Architecture](docs/architecture.md) — Engine pipeline and execution modes
- [Errors](docs/errors.md) — Error system and sandboxing behavior
