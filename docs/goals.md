# Project Goals

*Why GocciaScript exists and what it aims to be.*

## Executive Summary

- **Primary goal** — A sandboxed JavaScript runtime for AI agents: reduced attack surface with no ambient file system, network, or process access by default
- **Secondary goal** — A modern ECMAScript platform embeddable in desktop applications, enabling portable code that runs both on the desktop (via FreePascal) and on the web (via standard browsers)
- **Design philosophy** — Include only the parts of JavaScript that lead to clear and predictable code; exclude features that are error-prone, redundant, or widen the attack surface
- **Not a goal** — Full ECMAScript conformance, Node.js API compatibility, or server-side workloads

## Primary: Sandboxed Runtime for AI Agents

AI agents need to execute generated code in a constrained environment. GocciaScript provides a JavaScript execution environment that reduces the attack surface by default:

- **No ambient authority** — Scripts cannot access the file system, network, or operating system unless the host explicitly grants it through the embedding API
- **Sandboxed file system** *(not yet implemented)* — A virtual file system API that allows scripts to read and write files within a host-defined boundary, without access to the real file system
- **Read-only network access** — A WHATWG `fetch` (GET/HEAD only) gated by an explicit host allowlist (`--allowed-host`); without an allowlist any call to `fetch` throws `TypeError`, so scripts cannot retrieve data or reach the network unless the host opts in
- **Deterministic execution** — Same input produces the same output; no implicit global state leakage between executions
- **Timeout enforcement** — The `--timeout` flag and embedding API allow hosts to kill runaway scripts
- **Controlled built-in surface** — The host chooses which runtime globals to expose via `TGocciaRuntimeGlobals` (see [Embedding](embedding.md))
- **No eval** — `eval()` is excluded by design, preventing code injection from within scripts
- **No dynamic code generation by default** — The `Function()` constructor is excluded unless the host opts in via `--unsafe-function-constructor`; the `function` keyword (declarations and expressions) is excluded by default and only re-enabled in compatibility mode via `--compat-function`

The sandbox is a *reduced attack surface*, not a formally verified security boundary. The engine has not been audited by a third party. The sandboxing relies on the host not exposing dangerous APIs, on `eval` being unconditionally absent, and on the host not opting in to `--unsafe-function-constructor` (which would re-enable dynamic code generation via `Function()`).

## Secondary: Embeddable Desktop Platform

Desktop applications built with FreePascal (Lazarus, command-line tools, game engines) can embed GocciaScript to add scripting capabilities:

- **Portable code** — Scripts written for GocciaScript's ECMAScript subset run identically in a browser (since the subset is valid standard JavaScript). This enables sharing logic between a desktop app and a web frontend.
- **Single-binary deployment** — GocciaScript compiles into the host application with no external runtime dependencies
- **Custom globals** — The host injects application-specific APIs through `DefineLexicalBinding` and custom built-in types (see [Embedding](embedding.md) and [Adding Built-in Types](adding-built-in-types.md))
- **Cross-platform** — Runs on macOS, Linux, Windows, and FreeBSD via FreePascal's cross-compilation

## What GocciaScript is Not

- **Not a Node.js replacement** — No `require()`, no `node:` built-in modules, no event loop with I/O callbacks
- **Not aiming for 100% ECMAScript conformance** — Features excluded by design (`==`, `eval`, traditional loops) will not be added. `var` and the `function` keyword are excluded by default but available as opt-in compatibility toggles (`--compat-var`, `--compat-function`). Runtime enforcement of type annotations is also opt-in (`--strict-types`). See [Language](language.md) for the full list.
- **Not a formally verified sandbox** — The sandbox reduces attack surface but has not been independently audited
- **Not performance-competitive with V8/SpiderMonkey** — GocciaScript prioritizes correctness, embeddability, and reduced attack surface over raw throughput

## Related documents

- [Language](language.md) — The ECMAScript subset: what's supported, excluded, and why
- [Embedding](embedding.md) — How to embed GocciaScript in a FreePascal application
- [Architecture](architecture.md) — Engine pipeline and execution modes
- [Errors](errors.md) — Error system and sandboxing behavior
