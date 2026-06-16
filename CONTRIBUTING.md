# Contributing to GocciaScript

**This is the contributing guide for every contributor**—human or AI. One standard; no separate "agent rules" vs "human rules" for what may be merged.

## Guides

| Guide | What it covers |
|-------|---------------|
| [Workflow](docs/contributing/workflow.md) | Local setup, branch workflow, issues & PRs, verification |
| [Code Style](docs/contributing/code-style.md) | Pascal naming, constants, spec annotations, generics, hash maps, file organization |
| [Tooling](docs/contributing/tooling.md) | Auto-formatting, Lefthook, editor config, platform-specific pitfalls |

## Documentation

When your change affects behavior visible to users or contributors, update the relevant documentation:

| Change type | Update |
|-------------|--------|
| New language feature | [docs/language.md](docs/language.md) + [docs/built-ins.md](docs/built-ins.md) if it adds API surface |
| New built-in type | [docs/built-ins.md](docs/built-ins.md) + follow the [Adding Built-in Types](docs/adding-built-in-types.md) checklist |
| Architecture change | The relevant `docs/` file ([interpreter.md](docs/interpreter.md), [bytecode-vm.md](docs/bytecode-vm.md), [core-patterns.md](docs/core-patterns.md), etc.) |
| New design decision | ADR under [docs/adr/](docs/adr/) |
| User-facing feature | [README.md](README.md) |
| Workflow or style change | The relevant file under [docs/contributing/](docs/contributing/) |

**Documentation standards:**

- **Executive summary** — Every `docs/` file except ADR files must include a `## Executive Summary` heading with 3–6 bulleted key points, placed after the title and subtitle. Root-level files (README, CONTRIBUTING, AGENTS) are navigation entry points and are exempt. An italic subtitle alone does not satisfy this requirement.
- **No duplication** — Each topic has one authoritative document with the full detail. Other documents that reference it use a one-liner and link back. Do not maintain the same content in two places.
- **Spikes are snapshots** — Files under `docs/spikes/` are point-in-time investigation records. Do not update them after the initial creation; if findings change, add a new ADR and link to it.
- **Decision records are immutable** — Each ADR under `docs/adr/` records what was decided at that point in time. Do not retroactively update ADRs to match the current implementation. If the implementation changes, add a new ADR with the new decision. Links from ADRs to other docs may be updated if targets are renamed.

`docs/` files are about the GocciaScript project (engine, language, runtime). Contributing guides are about FreePascal contribution standards. Match the voice and structure of the target document.

## Readiness and Completion

Before implementation, use [DEFINITION_OF_READY.md](DEFINITION_OF_READY.md) to confirm the work is ready to start. Before handoff or PR, use [DEFINITION_OF_DONE.md](DEFINITION_OF_DONE.md) to confirm the change is complete.

## Implementation principles

- **Prefer modifying existing code** — Update, simplify, or replace the existing implementation before adding parallel code paths or new helpers.
- **Simplify and centralize** — Keep the smallest complete implementation. When the same logic is needed more than once, centralize it so behavior, validation, and error formatting do not drift. Centralization must follow the existing architecture, ownership boundaries, and project patterns in [Architecture](docs/architecture.md), [Core Patterns](docs/core-patterns.md), and [Code Style](docs/contributing/code-style.md); do not create a new abstraction that cuts across those boundaries without a clear design reason.
- **Verify against the official spec** — For ECMAScript behavior, check the current official ECMA-262 text before implementing or reviewing semantics. Use spec comments as described in [Code Style](docs/contributing/code-style.md#ecmascript-spec-annotations), but do not treat comments as a substitute for reading the spec. When behavior is ambiguous or bug reports appear to conflict with the spec, compare against other JavaScript engine implementations such as V8, SpiderMonkey, JavaScriptCore, or Node.js before deciding the expected behavior.

## Critical rules

These rules **must** be followed when modifying the codebase:

### 1. Evaluator Purity and VMT Dispatch

Evaluation is **pure** — same expression + context always produces the same result. State changes happen through scope and value objects, never evaluator-internal state. Expression and statement evaluation uses VMT dispatch on AST nodes. See [docs/interpreter.md](docs/interpreter.md) for the full evaluator model.

### 2. Scope Creation

`TGocciaScope` must **never** be instantiated directly. Always use the `CreateChild` factory method on an existing scope:

```pascal
// Correct
ChildScope := ParentScope.CreateChild(skBlock);

// WRONG — do not do this
ChildScope := TGocciaScope.Create(ParentScope, skBlock);
```

### 3. Testing Requirements

JavaScript end-to-end tests are the **primary** way of testing GocciaScript. When implementing a new feature or fixing a bug:

- Add JavaScript tests under the `tests/` directory, following the existing directory structure.
- **One method per file** — each test file focuses on a single method or operation.
- **Edge cases are co-located** — edge case tests belong in the same file as the happy-path tests.
- Always verify changes by running the all-executor JavaScript suite:

  ```bash
  ./build.pas testrunner
  ./build/GocciaTestRunner tests
  ./build/GocciaTestRunner tests --mode=bytecode
  ```

See [docs/testing.md](docs/testing.md) for the full testing guide including directory structure, naming conventions, and platform-specific rules.

### 4. Language Restrictions

GocciaScript intentionally excludes certain JavaScript features — do **not** add support for them. See [docs/language.md](docs/language.md) for the full list of supported and excluded features with rationale.

## Quick reference

See [AGENTS.md](AGENTS.md#quick-reference) for the full command reference, or [Build System](docs/build-system.md) for detailed documentation.

## Further reading

- **[README.md](README.md)** — What GocciaScript is, how to get started, and the [documentation index](README.md#documentation)
- **Engine docs** — [Architecture](docs/architecture.md), [Interpreter](docs/interpreter.md), [Bytecode VM](docs/bytecode-vm.md), [Core patterns](docs/core-patterns.md)
- **[AGENTS.md](AGENTS.md)** — Agent-only operating manual for coding assistants
