# Vision

*Why GocciaScript exists and what it aims to be.*

## Executive Summary

- **Primary goal** — An ECMAScript engine for embedding: today in FreePascal and Delphi applications, with a stable C ABI (`libgoccia`) and public API as the 1.0 surface
- **Defaults** — Sensible and modern out of the box, and easy to customise through a few engine knobs; there are no presets
- **Capabilities for use cases** — The runtime provides what different hosts need and makes it simple to install: Web APIs for React server rendering, a virtual file system, and Node.js compatibility to a degree
- **Conformance** — All of test262 in every mode it requires, strict and non-strict, Annex B and staging included; TC39 proposals are tracked from stage 1
- **Performance** — An interpreter faster than QuickJS first, then `.gcbundle` bytecode artifacts, an opt-in baseline JIT and a compiled executable
- **Reference host** — `GocciaRunner`'s sandbox mode, a runtime for code that AI agents generate, is an example of what an embedder can build on the capability model
- **Toolchain** — A Vitest-compatible test runner, and `goccia:ast` as a platform for formatters and codemods
- **1.0** — A stable C ABI and public API, all of test262, an interpreter faster than QuickJS, and `.gcbundle`
- **Not a goal** — Node.js or browser host environments as the default surface, presets, or unbounded server workloads

## Primary: an Engine for Embedding

Applications embed GocciaScript to add scripting. Today the host is a FreePascal or Delphi application: Lazarus programs, command-line tools, game engines. A stable C ABI, the planned `libgoccia` library ([#823](https://github.com/frostney/GocciaScript/issues/823)), will open the engine to hosts in other languages; together with a named public Pascal API ([#2050](https://github.com/frostney/GocciaScript/issues/2050)), it is the surface 1.0 keeps stable.

- **Single-binary deployment** — GocciaScript compiles into the host application with no external runtime dependencies beyond OS-provided libraries: OpenSSL for HTTPS on Linux (macOS and Windows use their system TLS) and ICU for `Intl` when it is present
- **No ambient authority** — An engine's [capability set](docs/permissions.md) is `None` by default, so scripts reach the file system, network, operating system or native FFI only through grants the host gives; the command-line hosts expose the same model as `--allow-read`, `--allow-net`, `--allow-ffi` and the other grants
- **Controlled runtime surface** — The host chooses which runtime extensions or profiles to install, and injects its own APIs through `DefineLexicalBinding` and custom built-in types (see [Embedding](docs/embedding.md) and [Adding Built-in Types](docs/adding-built-in-types.md))
- **Execution limits** — A host bounds a run with a timeout, an instruction limit, a memory ceiling and a call-stack depth limit, and can stop it from another thread
- **Isolated engines** — For the same source, configuration and host-provided runtime surface, core-language execution is repeatable, and intrinsic state does not leak between engine instances; hosts that install time, network or other external runtime extensions own those sources of nondeterminism
- **Portable code** — Scripts written for the defaults use modern standard JavaScript forms and can be shared with code for other engines, browsers included
- **Cross-platform** — Prebuilt and tested on macOS and Linux (x64 and arm64) and Windows (x64 and x86); FreePascal's cross-compilation can reach further targets, such as FreeBSD, that CI does not build or test

## Sensible Defaults, Easy to Customise

An engine starts with modern defaults: the recommended language profile (arrow functions, classes, modules, strict semantics, `let`/`const`), no dynamic code generation, and no capabilities. They are the starting point for new code, not a limit on what the engine supports. A host that needs more changes the defaults it needs:

- **`Engine.Compatibility`** — The ECMAScript forms the recommended profile leaves out, such as `var`, the `function` keyword, `arguments`, non-strict semantics, loose equality, labels, traditional loops and `for...in`, and new Annex B surfaces ([ADR 0177](docs/adr/0177-annex-b-is-part-of-the-test262-conformance-target.md)); the command-line hosts expose each as a `--compat-*` flag
- **`Engine.FunctionConstructor.Enabled`** — The `Function()` constructor (`--unsafe-function-constructor`)
- **`EnableShadowRealm`** — The `ShadowRealm` global (`--unsafe-shadowrealm`)
- **The eval option** — `eval()`, through the engine API only, with no command-line flag or config key ([#2065](https://github.com/frostney/GocciaScript/issues/2065))

Compatibility flags are supported customisation for running the whole of ECMAScript, not escape hatches. There are no presets: a host sets the knobs its scripts need. See [Language](docs/language.md) for the profile and every compatibility path, and [Embedding](docs/embedding.md) for the engine options.

## Capabilities for Different Use Cases

Embedding means that different hosts need different things. GocciaScript provides those capabilities as runtime extensions a host installs, and aims to make each use case simple:

- **React server rendering** — The Web APIs that a server renderer such as React's relies on, for example `MessageChannel`, `ReadableStream` and `crypto`, are in scope ([#1562](https://github.com/frostney/GocciaScript/issues/1562)). The runtime already provides `fetch`, `URL`, `TextEncoder`/`TextDecoder`, `Blob`, `AbortController` and `EventTarget` (see [Built-ins](docs/built-ins.md))
- **A virtual file system** — A bounded, in-memory file system that the host fills and reads back, exposed to scripts as an `fs` module (see [Embedding — Sandbox filesystem](docs/embedding.md#sandbox-filesystem))
- **Node.js compatibility, to a degree** — GocciaScript leans into the parts of Node compatibility that are fairly easy to provide, such as `node:async_hooks` and opt-in ESM `node_modules` resolution (`--allow-import=node_modules`), as capabilities a host installs rather than the default surface
- **Web-interoperable runtimes** — Aligning selected runtime globals with WinterTC's Minimum Common Web API and the WHATWG standards remains a direction, taken where it serves an embedding use case

## Conformance

GocciaScript aims to pass all of test262 in every mode test262 requires, strict and non-strict, with Annex B and staging included, as the [Annex B decision record](docs/adr/0177-annex-b-is-part-of-the-test262-conformance-target.md) sets out. The generated reports on the [compatibility dashboard](https://www.gocciascript.dev/compatibility) measure progress. test262's metadata is taken as correct: where the runner's handling of a test diverges from it, the runner is at fault and is fixed, as it was for [harness includes](docs/adr/0175-test262-harness-includes-are-separate-non-strict-scripts.md).

GocciaScript also tracks TC39 proposals, including stage 1 and stage 2 proposals that test262 does not cover yet; see [Language Tables — TC39 Proposals](docs/language-tables.md#tc39-proposals).

## Performance

Speed is part of what an embeddable engine offers, and the targets come in order ([ADR 0176](docs/adr/0176-the-interpreter-beats-quickjs-before-gcbundle-a-jit-and-a-compiled-executable.md)):

1. **An interpreter faster than QuickJS** on the AWFY and JetStream geomeans, on every shipped platform
2. **`.gcbundle` bytecode artifacts**, so a host can ship compiled code instead of source
3. **An opt-in baseline JIT** (1.1.0), with the interpreter as the tier of record
4. **A compiled executable** (1.2.0)

The [Performance Barometer](docs/benchmarks.md) measures progress against QuickJS.

## What 1.0 Means

1.0 is a stable C ABI and public API, all of test262, an interpreter faster than QuickJS, and `.gcbundle`. The JIT and the compiled executable come after it, in 1.1.0 and 1.2.0.

## Reference Host: a Sandbox for AI Agents

AI agents need to run the code they generate in a constrained environment. `GocciaRunner`'s sandbox mode is that environment, and it is built on what the engine offers any embedder: the capability model, the virtual file system and the runtime extensions. It is a reference host and an example of what an embedder can build, not the limit of the engine:

- **A file system of copied inputs** — Scripts see a bounded virtual file system filled only from inputs the caller copies in, with no ambient host-filesystem access, and written back only for inputs copied with `--copy-rw` (see [Permissions — Sandbox mode](docs/permissions.md#sandbox-mode))
- **Read-only network access** — A WHATWG `fetch` (GET/HEAD only) gated by the engine's `net` capability (`--allow-net`); without a grant any call to `fetch` throws `PermissionDenied`, and private or loopback destinations stay refused unless the grant names them (`--allow-net=private` or an explicit address)
- **Timeout enforcement** — The `--timeout` option kills runaway scripts
- **No dynamic code generation by default** — Normal runtime hosts do not install `eval()`, the `Function()` constructor stays disabled unless the host opts in via `--unsafe-function-constructor`, and `ShadowRealm`, whose `prototype.evaluate` performs dynamic source evaluation, is registered only under `--unsafe-shadowrealm`; the private test262 host exposes a conformance-only eval hook
- **Modern function syntax by default** — Arrow functions, methods, accessors and class methods are the recommended forms; the `function` keyword is available only through `--compat-function`

The sandbox is a *reduced attack surface*, not a formally verified security boundary. The engine has not been audited by a third party. The sandboxing relies on the host not exposing dangerous APIs, on `eval` being absent from normal runtimes, on the test262 eval hook remaining private to `GocciaTest262Runner`, and on the host not opting in to `--unsafe-function-constructor` or `--unsafe-shadowrealm` (either of which would re-enable dynamic code generation, via `Function()` and `ShadowRealm.prototype.evaluate` respectively).

## Toolchain

### A Vitest-Compatible Test Runner

Testing is where an engine meets ordinary developer work. The built-in runner therefore targets the API developers already write — `describe`/`test`/`expect`, lifecycle hooks, and mocks — and treats Vitest as the reference for what those names *mean*, not merely what they are called.

- **Behavior checked against the reference, not against its documentation** — Equality, `Set`/`Map`, error, and `toThrow` semantics are probed against a pinned Vitest release and reconciled; where GocciaScript still differs, the divergence is deliberate and recorded rather than discovered by users
- **Agreement is gated, not asserted** — Shared differential suites run against an external JavaScript runtime as the semantics oracle in continuous integration, so a drift in either direction fails a build instead of quietly changing what tests mean
- **One binary, no host toolchain** — Suites run without a Node.js installation, package resolution, or a transform step in front of them. That is a claim about what a run has to do; engine speed is covered under [Performance](#performance)
- **A drop-in replacement is the direction, not a finished claim** — A full drop-in audit is still ahead, but importing `vi` from `"vitest"` now resolves to a bundled compatibility shim covering `vi.fn`, `vi.spyOn`, factory-form `vi.mock` (a synchronous arrow factory returning an object literal; no automock, no spread-based partial mock), `vi.unmock`, `vi.mocked`, `vi.stubGlobal`/`vi.unstubAllGlobals`, `vi.clearAllMocks`/`vi.resetAllMocks`/`vi.restoreAllMocks`, and the fake-timer family over a deterministic virtual timer queue, so suites built on those members run unchanged; `vi.stubEnv`/`vi.unstubAllEnvs` work too, but over a host-injected `process.env`, since the engine has no `process` of its own. The members that are not implemented — `vi.waitFor`/`vi.waitUntil`, `vi.hoisted`, `vi.doMock`/`vi.doUnmock`/`vi.resetModules`, `vi.importActual`/`vi.importMock`, and `vi.setConfig`/`vi.resetConfig` — throw a named, actionable error rather than failing quietly; see [Test Framework API](docs/testing-api.md) for the current divergence list

### `goccia:ast` for Formatters and Codemods

`goccia:ast` gives scripts the tree, tokens and comments that GocciaScript's own parser produces, and it is a platform the project supports for formatters and codemods. It is experimental today, behind `--experimental-ast`, and its shape may still change between releases; see [AST Built-ins](docs/built-ins-ast.md).

## What GocciaScript is Not

- **Not a Node.js runtime** — Node compatibility is a set of capabilities a host installs, not the default surface, and GocciaScript does not aim to reproduce Node's whole host environment. CommonJS and the general `node:` module set are not provided. Node-authored scripts run when they use standard ECMAScript or the capabilities above
- **Not a browser host environment** — GocciaScript takes on Web APIs where they serve an embedding use case, such as the ones React server rendering needs, but does not aim to provide a DOM, Web Workers, storage APIs or a browser event loop
- **Not a set of presets** — The defaults and the knobs that change them are the whole configuration; GocciaScript does not bundle them into named modes
- **Not a formally verified sandbox** — The sandbox reduces attack surface but has not been independently audited

## Related documents

- [Language](docs/language.md) — ECMAScript support, recommended defaults, compatibility flags, and shims
- [Embedding](docs/embedding.md) — How to embed GocciaScript in a FreePascal application
- [Permissions](docs/permissions.md) — The capability model, grants and sandbox mode
- [Architecture](docs/architecture.md) — Engine pipeline and main layers
- [test262](docs/test262.md) — How conformance is measured
- [Benchmarks](docs/benchmarks.md) — How performance is measured
- [Errors](docs/errors.md) — Error system and sandboxing behavior
