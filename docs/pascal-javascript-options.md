# JavaScript Engine Options for FreePascal and Delphi

*How to evaluate GocciaScript and other JavaScript integration shapes without confusing language support, default policy, host APIs, and compiler support.*

## Executive Summary

- **GocciaScript's position** — A FreePascal-native, embeddable ECMAScript runtime, not a separate JavaScript-like language
- **Recommended profile** — Selected legacy or high-risk forms are disabled by default; compatibility flags enable many standard legacy forms for conformance and existing code
- **Host compatibility is separate** — ECMAScript support does not imply Node.js, npm, browser, DOM, or Web API compatibility
- **Compiler status is separate** — FreePascal is supported today; Delphi compiler support is currently untested
- **Current evidence** — Use the live [ECMAScript compatibility dashboard](https://www.gocciascript.dev/compatibility) rather than copying a pass rate into comparison documents

## Start With Four Separate Questions

JavaScript engine comparisons often collapse four different concerns into one word: "compatibility". Keep them separate:

| Dimension | Question |
|-----------|----------|
| Language capability | Which ECMAScript syntax, semantics, and built-ins does the engine implement? |
| Default policy | Which implemented forms are enabled in the recommended out-of-the-box profile? |
| Host environment | Does the host provide Node.js, npm, browser, filesystem, network, or application-specific APIs? |
| Pascal toolchain | Is the integration supported with FreePascal, the Delphi compiler, or both? |

A limitation in one dimension is not automatically a limitation in the others. In particular, not providing Node.js or npm APIs does not mean that an engine only implements a small JavaScript language subset.

## Where GocciaScript Fits

GocciaScript is designed for applications that want an ECMAScript engine implemented in FreePascal, with:

- direct FreePascal embedding through `TGocciaRuntime` or `TGocciaEngine`
- tree-walk and bytecode execution modes
- host-controlled runtime extensions and virtual modules
- sandbox-first recommended defaults
- generated test262 reporting on the main branch

The recommended profile prefers modern, explicit forms such as `let`, `const`, arrow functions, classes, modules, and strict equality. Selected legacy or high-risk forms are disabled by default. Many standard legacy forms are implemented behind explicit compatibility flags because the default profile is product policy, not the engine's language ceiling.

See [Language](language.md) for the authoritative policy and [Embedding the Engine](embedding.md) for the FreePascal API.

## The Main Integration Shapes

The right option depends on the boundary your application needs:

| Integration shape | Fits when | Main consequence |
|-------------------|-----------|------------------|
| FreePascal-native engine | You want source-level Pascal integration and a self-contained engine | Language and host capabilities come from that engine's own implementation |
| Foreign engine binding | You need an existing native engine through a C or C++ ABI | Deployment, ownership, callbacks, and ABI safety become part of the integration |
| Separate runtime process | You need an existing runtime and its package ecosystem | Process lifecycle, IPC, permissions, and distribution become part of the application |
| Browser component | You need DOM and browser APIs rather than only ECMAScript | The browser becomes the host environment and security boundary |

GocciaScript occupies the first shape. It is not intended to imitate Node.js or a full browser host.

## When GocciaScript Is A Strong Candidate

Consider GocciaScript when:

- the supported host is FreePascal
- JavaScript runs as embedded application logic, automation, plugins, or generated agent code
- the application wants explicit control over available modules and globals
- avoiding a foreign engine ABI or external runtime process matters
- the live compatibility dashboard covers the language surface the application requires

## When To Choose A Different Host Shape

Choose another integration shape when the requirement is specifically:

- unmodified Node.js packages or `node:` built-ins
- npm package resolution and Node.js globals
- a DOM, browser event loop, Web Workers, or browser storage
- confirmed support for compiling the engine itself with the Delphi compiler

These are host or toolchain requirements. They should not be reported as missing ECMAScript language features.

## FreePascal And Delphi

GocciaScript is implemented in Object Pascal and built with FreePascal in Delphi-compatible language mode. FreePascal is the supported compiler, build system, and embedding path.

The Delphi compiler is not currently a tested target. "Written in Delphi mode" therefore means the source uses that FreePascal language mode; it does not constitute a current Delphi compiler support guarantee.

## Verify A Decision

Use first-party evidence for the dimension being evaluated:

1. Check the live [ECMAScript compatibility dashboard](https://www.gocciascript.dev/compatibility) for current generated test262 results.
2. Check [Language](language.md) and [Language Tables](language-tables.md) for recommended defaults, compatibility flags, proposals, and exclusions.
3. Check [Embedding the Engine](embedding.md) for the supported FreePascal integration API.
4. Check [Host Environment](host-environment.md), [Virtual Modules](virtual-modules.md), and [FFI Built-ins](built-ins-ffi.md) for the capabilities a host can explicitly provide.

Avoid copying a current compatibility percentage into static comparison text. The dashboard is the authoritative, dated, commit-linked source.
