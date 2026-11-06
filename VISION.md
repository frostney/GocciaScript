# Vision

*Why GocciaScript exists and what it aims to be.*

## Executive Summary

- **Primary goal** — An ECMAScript engine for embedding: today in FreePascal and Delphi applications, with a stable C ABI (`libgoccia`) and public API as the 1.0 surface
- **Defaults** — Sensible and modern out of the box, and easy to customize through a few engine knobs; there are no presets
- **Runtime extensions for use cases** — The runtime provides what different hosts need and makes it simple to install: Web APIs for React server rendering, a virtual filesystem, and Node.js compatibility to a degree
- **Conformance** — All of test262 in every mode it requires, strict and non-strict, Annex B and staging included; TC39 proposals are tracked from stage 1
- **Performance** — An interpreter faster than QuickJS first, then `.gcbundle` bytecode artifacts, an opt-in baseline JIT and a compiled executable
- **Reference host** — `GocciaRunner`'s sandbox mode, a runtime for code that AI agents generate, is an example of what an embedder can build on the capability model
- **Toolchain** — A Vitest-compatible test runner, and `goccia:ast` as a platform for formatters and codemods
- **1.0** — A stable C ABI and public API, all of test262, an interpreter faster than QuickJS, and `.gcbundle`
- **Not a goal** — Node.js or browser host environments as the default surface, or unbounded server workloads

## Primary: an Engine for Embedding

Applications embed GocciaScript to add scripting. Today the host is a FreePascal or Delphi application: Lazarus programs, command-line tools, game engines. A stable C ABI, the planned `libgoccia` library ([#823](https://github.com/frostney/GocciaScript/issues/823)), will open the engine to hosts in other languages; together with a named public Pascal API ([#2050](https://github.com/frostney/GocciaScript/issues/2050)), it is the surface 1.0 keeps stable.

- **Single-binary deployment** — GocciaScript compiles into the host application with no external runtime dependencies beyond OS-provided libraries: OpenSSL for HTTPS on Linux (macOS and Windows use their system TLS) and, when it is present, ICU for `Intl` and Unicode data
- **No ambient authority** — An engine's [capability set](docs/permissions.md) is `None` by default, so beyond the files of its own project's static module graph a script reaches the filesystem, network or native FFI only through grants the host gives; the command-line hosts expose the same model as `--allow-read`, `--allow-net`, `--allow-ffi` and the other grants
- **Controlled runtime surface** — The host chooses which runtime extensions or profiles to install, and injects its own APIs through `DefineLexicalBinding` and custom built-in types (see [Embedding](docs/embedding.md) and [Adding Built-in Types](docs/adding-built-in-types.md))
- **Execution limits** — A host bounds a run with a timeout, an instruction limit, a memory budget and a call-stack depth limit, and can stop it from another thread. The memory budget gates individual allocations and bounds peak allocation requests, not total resident memory, so a host that needs a hard ceiling also sets an operating-system limit (see [Embedding — Execution Limits](docs/embedding.md#execution-limits) and [Garbage Collector — What the gate does not bound](docs/garbage-collector.md#gated-growth-points))
- **Isolated engines** — Intrinsic state does not leak between engine instances. Core-language execution is repeatable for the same source, configuration and host-provided runtime surface; `Date.now()`, `Math.random()` and the other time and random sources come from host providers, which the host fixes with the [deterministic profile](docs/host-environment.md#deterministic-profile) or its own providers, and hosts that install network or other external runtime extensions own those sources of nondeterminism
- **Portable code** — Scripts written for the defaults use modern standard JavaScript forms, which other engines, browsers included, run as well, once they leave out any type annotations (the engine accepts them by default as [comments](docs/type-annotations.md), and browsers do not yet) and any GocciaScript-specific runtime extensions
- **Cross-platform** — Prebuilt and tested on macOS and Linux (x64 and arm64) and Windows (x64 and x86); FreePascal's cross-compilation can reach further targets, such as FreeBSD, that CI does not build or test

## Sensible Defaults, Easy to Customize

An engine starts with modern defaults: the recommended language profile (arrow functions, classes, modules, strict semantics, `let`/`const`) and no dynamic code generation. They are the starting point for new code, not a limit on what the engine supports. A host changes the ones it needs with these knobs:

- **`Engine.Compatibility`** — The ECMAScript forms the recommended profile leaves out, and new Annex B surfaces ([ADR 0177](docs/adr/0177-annex-b-is-part-of-the-test262-conformance-target.md)); the command-line hosts expose each as a `--compat-*` flag
- **`Engine.FunctionConstructor.Enabled`** — The `Function()` constructor (`--unsafe-function-constructor`)
- **`EnableShadowRealm`** — The `ShadowRealm` global (`--unsafe-shadowrealm`)
- **A planned eval option** — `eval()`, through the engine API only, with no command-line flag or config key ([#2065](https://github.com/frostney/GocciaScript/issues/2065))

Compatibility flags are supported customization for running the whole of ECMAScript, not escape hatches. What scripts may reach outside the process is a separate setting, the capability set the host grants. See [Language](docs/language.md) for the profile and every compatibility path, and [Embedding — Changing the Defaults](docs/embedding.md#changing-the-defaults) for the knobs together.

## Runtime Extensions for Different Use Cases

Embedding means that different hosts need different things. GocciaScript provides them as runtime extensions and grants a host chooses, and aims to make each use case simple.

One test decides what goes in: GocciaScript provides a capability when it fits this vision and cannot be shimmed in userland, because it needs the engine or the host. What userland or an npm package can shim stays out of the engine, since a script can already bring it. Today's examples of what goes in are `fetch`, which reaches the network through the engine's `net` capability and the platform TLS stack; the `fs` module over a virtual filesystem that the host fills and reads back; and `node:async_hooks`, whose `AsyncLocalStorage` relies on the engine propagating async context across `await` and promise reactions.

- **React server rendering** — The Web APIs that a server renderer such as React's relies on, for example `MessageChannel`, `ReadableStream` and `crypto`, are planned for this use case ([#1562](https://github.com/frostney/GocciaScript/issues/1562)); none of them ships today, and each goes into the engine only if it passes this test. The runtime already provides `fetch`, `URL`, `TextEncoder`/`TextDecoder`, `Blob`, `AbortController` and `EventTarget` (see [Built-ins](docs/built-ins.md))
- **A virtual filesystem** — A bounded, in-memory filesystem that the host fills and reads back, exposed to scripts as an `fs` module (see [Embedding — Sandbox filesystem](docs/embedding.md#sandbox-filesystem))
- **Node.js compatibility, to a degree** — GocciaScript leans into the parts of Node compatibility that are fairly easy to provide and need the engine or host, such as `node:async_hooks` (part of the loader runtime profile), a Node-compatible `fs` over the virtual filesystem, and opt-in ESM `node_modules` resolution (an `import` grant, `--allow-import=node_modules`), each of which a host chooses, rather than a Node host environment
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

AI agents need to run the code they generate in a constrained environment. `GocciaRunner`'s sandbox mode is that environment, and it is built on what the engine offers any embedder: the capability model, the virtual filesystem and the runtime extensions. It is a reference host and an example of what an embedder can build, not the limit of the engine. It runs on the engine's defaults above and adds:

- **A filesystem of copied inputs** — Scripts see a bounded virtual filesystem filled only from inputs the caller copies in, with no ambient host-filesystem access, and written back only for inputs copied with `--copy-rw`
- **Network as the only grant** — `net` is the one capability sandbox mode can grant: a WHATWG `fetch` (GET/HEAD only) that throws `PermissionDenied` without an effective `net` grant (`--allow-net`, or a trusted config), and that refuses private or loopback destinations unless the grant names them

See [Permissions — Sandbox mode](docs/permissions.md#sandbox-mode) for the full contract.

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

- **Not a Node.js runtime** — Node compatibility is a set of runtime extensions a host installs, not the default surface. Under the test in [Runtime Extensions for Different Use Cases](#runtime-extensions-for-different-use-cases), a `node:` module ships when it needs the engine or host, as `node:async_hooks` does, and whatever userland can shim stays out. Node-authored scripts run when they use standard ECMAScript or the runtime extensions listed under [Runtime Extensions for Different Use Cases](#runtime-extensions-for-different-use-cases)
- **Not a browser host environment today** — GocciaScript takes on Web APIs where they serve an embedding use case and pass the same test, such as the ones React server rendering needs, but it does not currently provide a DOM, Web Workers, storage APIs, a browser event loop or a full browser host environment
- **Not a set of presets** — A host sets the language and dynamic-code knobs one by one; GocciaScript does not bundle them into named modes
- **Not a formally verified sandbox** — The sandbox reduces attack surface but has not been independently audited

## Related documents

- [Language](docs/language.md) — ECMAScript support, recommended defaults, compatibility flags, and shims
- [Embedding](docs/embedding.md) — How to embed GocciaScript in a FreePascal application
- [Permissions](docs/permissions.md) — The capability model, grants and sandbox mode
- [Architecture](docs/architecture.md) — Engine pipeline and main layers
- [test262](docs/test262.md) — How conformance is measured
- [Benchmarks](docs/benchmarks.md) — How performance is measured
- [Errors](docs/errors.md) — Error system and sandboxing behavior
