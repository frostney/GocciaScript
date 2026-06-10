# Project Goals

*Why GocciaScript exists and what it aims to be.*

## Executive Summary

- **Primary goal** — A sandboxed JavaScript runtime for AI agents: reduced attack surface with no ambient file system, network, or process access by default
- **Secondary goal** — A modern ECMAScript platform embeddable in desktop applications, enabling portable code that runs both on the desktop (via FreePascal) and on the web (via standard browsers)
- **Design philosophy** — Default to the parts of JavaScript that lead to clear and predictable code; keep legacy and high-risk behavior opt-in
- **Release-track objective** — Reach full ECMAScript conformance as closely as possible for the next major version, measured by the generated test262 reports
- **Not a goal** — Node.js API compatibility, browser API compatibility, or server-side workloads

## Primary: Sandboxed Runtime for AI Agents

AI agents need to execute generated code in a constrained environment. GocciaScript provides a JavaScript execution environment that reduces the attack surface by default:

- **No ambient authority** — Scripts cannot access the file system, network, or operating system unless the host explicitly grants it through the embedding API
- **Sandboxed file system** *(not yet implemented)* — A virtual file system API that allows scripts to read and write files within a host-defined boundary, without access to the real file system
- **Read-only network access** — A WHATWG `fetch` (GET/HEAD only) gated by an explicit host allowlist (`--allowed-host`); without an allowlist any call to `fetch` throws `TypeError`, so scripts cannot retrieve data or reach the network unless the host opts in
- **Deterministic execution** — Same input produces the same output; no implicit global state leakage between executions
- **Timeout enforcement** — The `--timeout` option and embedding API allow hosts to kill runaway scripts
- **Controlled runtime surface** — The host chooses which runtime extensions or profiles to install (see [Embedding](embedding.md))
- **No eval by default** — `eval()` is excluded from normal runtimes, preventing code injection from within scripts; the private test262 host exposes a conformance-only eval hook
- **No dynamic code generation by default** — The `Function()` constructor is excluded unless the host opts in via `--unsafe-function-constructor`; the `function` keyword (declarations and expressions) is excluded by default and only re-enabled in compatibility mode via `--compat-function`

The sandbox is a *reduced attack surface*, not a formally verified security boundary. The engine has not been audited by a third party. The sandboxing relies on the host not exposing dangerous APIs, on `eval` being unconditionally absent, and on the host not opting in to `--unsafe-function-constructor` (which would re-enable dynamic code generation via `Function()`).

## Secondary: Embeddable Desktop Platform

Desktop applications built with FreePascal (Lazarus, command-line tools, game engines) can embed GocciaScript to add scripting capabilities:

- **Portable code** — Scripts written with GocciaScript's recommended defaults use modern standard JavaScript forms and can be shared with browser code. Hosts can opt into compatibility behavior when they need older ECMAScript semantics.
- **Single-binary deployment** — GocciaScript compiles into the host application with no external runtime dependencies
- **Custom globals** — The host injects application-specific APIs through `DefineLexicalBinding` and custom built-in types (see [Embedding](embedding.md) and [Adding Built-in Types](adding-built-in-types.md))
- **Cross-platform** — Runs on macOS, Linux, Windows, and FreeBSD via FreePascal's cross-compilation

## What GocciaScript is Not

- **Not a Node.js replacement** — No `require()`, no `node:` built-in modules, no event loop with I/O callbacks
- **Not browser API compatible** — Web-platform APIs appear only when they are explicitly implemented or installed by a runtime profile
- **Not a single fixed runtime surface** — The recommended defaults are sandbox-first and modern, but the engine and runtime are customizable. Compatibility flags exist primarily for ECMAScript conformance and legacy semantic requirements; userland code should usually prefer the default forms instead of enabling flags preemptively. Runtime enforcement of type annotations is also opt-in (`--strict-types`). See [Language](language.md) for the full list.
- **Not a formally verified sandbox** — The sandbox reduces attack surface but has not been independently audited
- **Not performance-competitive with V8/SpiderMonkey** — GocciaScript prioritizes correctness, embeddability, and reduced attack surface over raw throughput

## Related documents

- [Language](language.md) — ECMAScript support, recommended defaults, compatibility flags, and shims
- [Embedding](embedding.md) — How to embed GocciaScript in a FreePascal application
- [Architecture](architecture.md) — Engine pipeline and execution modes
- [Errors](errors.md) — Error system and sandboxing behavior
