---
name: native-nostalgia-stack
description: >-
  Applies the user's FreePascal toolchain and its build, formatting, hook, and
  test contracts while leaving project-specific mechanics local. Use when
  scaffolding or working in a FreePascal project that follows this toolchain.
license: Unlicense OR MIT
compatibility: >-
  Assumes a FreePascal project compiled with FPC in Delphi mode, with Lefthook
  for pre-commit hooks.
---

# Native nostalgia stack

## Instructions

This skill describes the **toolchain contract and project-specific structure** for a FreePascal project on this stack. Universal repo layout, AGENTS.md shape, docs template, pre-commit hook contract, scripts directory, agent-skill symlinks, and changelog generation live in `project-structure/SKILL.md`. Implementation rules for any individual project (engine internals, spec compliance, runtime configuration shape, etc.) belong in that project's own AGENTS.md and docs.

### Principles

- **Smallest viable toolchain.** Pick the smallest set of tools that satisfy the build-system and formatter contracts below. Do not pull in additional toolchains (CMake, Bazel, Gradle, etc.) when a smaller mechanism already satisfies the contract.
- **Verify versions at implementation:** the agent confirms the FPC version and any pinned tool versions in use at the moment of work — not from memory.

### Compiler

- **FreePascal (FPC), latest stable.** Verify the version pinned in `docs/tooling.md` (or the equivalent) and CI workflows before assuming it.
- **Delphi mode by default.** Delphi mode is the default unless the project explicitly pins another (`objfpc`, `mac`, `tp`). When a project pins another mode, that pin lives in `docs/code-style.md`.
- **Compiler flags live in a shared include file**, not inline in every unit. The project keeps a single `.inc` file (e.g. `source/<project>.inc`) holding `{$mode delphi}`, `{$H+}`, and any other project-wide directives. Every unit pulls the include via `{$I <project>.inc}` (or the project's chosen path); the directives are not repeated in each unit. Changing a project-wide flag means editing the include, not every file.

### Code style starting points

Enforced by the formatter where possible, otherwise by review.

- **Naming.**
  - Classes `T<Name>`, interfaces `I<Name>`, exceptions `E<Name>`.
  - Private fields use the `F` prefix.
  - Parameters with two or more letters use the `A` prefix; single-letter parameters (`A`, `B`, `E`, `T`) keep their name.
  - Functions, procedures, methods, locals, and constants use `PascalCase`. No underscores. No numeric suffixes (`PrimaryScope`, not `Scope1`).
  - **No abbreviations.** Full words for class, function, method, and type names. Industry-standard acronyms (`AST`, `JSON`, `ISO`, `UUID`, `URL`, `HTTP`, etc.) are kept as-is.
- **OOP by default.** Classes are the modeling primitive: each domain concept gets its own class with explicit responsibilities, and variation is expressed through inheritance and polymorphism. Reach for records, plain procedures, or generic data structures only when there is no behavior to attach.
- **Mark small subroutines `inline` where it makes sense.** Add `inline;` to small functions, procedures, and methods (trivial accessors, hot-path one-liners). `inline` is a compiler hint — FPC decides whether to actually inline based on size and content; the directive nudges it without forcing it. Skip `inline` on large bodies, recursive routines, and routines that take their own address.
- **`const` parameters by default.** Use `var` or `out` only when the parameter is mutated. `const` applies to objects, strings, records, integers — anything not intentionally written.
- **No magic literals.** Bare numeric and string literals are extracted into named constants and declared in `interface` when shared between `interface` and `implementation`.
- **Uses clauses: one unit per line, alphabetized within groups, blank line between groups.** Group order: system units → third-party / non-prefixed project units → namespaced project units → relative-path units. Enforced by the formatter.
- **File organization.** `interface` declares only the public API. Heavy or cycle-causing dependencies go in the `implementation uses` clause to break circular references.
- **Generic specializations have named aliases.** When a generic specialization (`TObjectList<TFoo>`, `TOrderedMap<K,V>`, etc.) is used across more than one unit, declare a single named alias in the unit that owns the parameter type. Do not re-specialize the same generic locally — separate VMTs cause cross-unit type-cast failures under strict object checks.
- **RTTI via `TypInfo` when introspection is needed.** When a class needs to be inspected, configured, or dispatched dynamically at runtime (serialization, configuration binding, generic forms, scripted property access, etc.), expose the relevant properties and methods in the `published` section and read or write them through FPC's `TypInfo` (`GetPropInfo`, `GetPropValue`, `SetPropValue`, `GetMethodProp`, `SetMethodProp`, etc.). Do not put what does not need introspection into `published`.
- **Minimal public API.** Units expose only what's needed; implementation details stay inside `implementation`.

### Build system contract

Every project on this stack has a build system. The **implementation is the project's choice** (a Pascal program compiled by FPC, a Makefile, a Justfile, a shell script, etc.) as long as it satisfies this contract:

- **Single entry point** invoked from the repo root. The same command — whatever it is — drives every build.
- **Default target.** The bare invocation performs a clean dev build of every binary the project produces.
- **Named targets** per binary, addressable as positional arguments. Multiple targets can be passed at once.
- **Dev / prod distinction.** A `--dev` flag is the default (fast feedback); a `--prod` flag switches every step to release flags.
- **`clean` is a target,** not a separate script. A bare `clean` cleans everything; `clean <target>` cleans then builds that target.
- **Single output directory.** All binaries land under `build/` at the repo root. `build/` is never committed.

The choice of implementation, the list of targets, and the available flags are recorded in the project's `docs/build-system.md` (or equivalent). Read that file before adding or renaming a target.

### Formatter / linter contract

Every project on this stack has a formatter / linter that combines style rewriting and lint-style checks. The **implementation is the project's choice** (a Pascal program, `ptop`, `fpcformat`, a wrapper around an external tool, etc.) as long as it satisfies this contract:

- **No-flag invocation rewrites Pascal sources in place** to the project's canonical style.
- **`--check` flag exits non-zero on any deviation** without modifying files. This is the form CI and the pre-commit hook call.
- **Project-specific style rules are encoded inside the tool**, not scattered across config files. Adding or changing a rule means updating the formatter, recorded in `docs/code-style.md`.
- **Lint and format are one step** from the contributor's perspective. There is no "run lint, then run formatter" sequence; one command covers both.

### Codebase health contract

Every project on this stack runs a codebase-health check that surfaces structural risk. The **implementation is the project's choice** (a Pascal program, a wrapper around external tooling, etc.) as long as it satisfies this contract:

- **Duplication is reported** — copy-paste between units and within a single unit, above a project-defined size threshold. Output names the duplicated regions and their locations.
- **Per-function complexity is reported** using cyclomatic and cognitive complexity metrics. Functions exceeding project-defined thresholds are flagged.
- **Per-file health is reported** as a single aggregate signal contributors can sort on.
- **Hotspots are surfaced** by combining structural findings with git churn, so refactor effort lands on code that is both complex and frequently changed.
- **Non-zero exit on threshold breach.** When any project-configured threshold is violated, the check exits non-zero so CI and the pre-commit hook can enforce it.
- **Single config file at the repo root** holds the thresholds and any ignores. Tweaking the bar means editing that one file.

The choice of implementation and the configured thresholds are recorded in the project's `docs/tooling.md` (or equivalent). The check is wired into the project's verification gate alongside the formatter and build-and-test steps.

### Pre-commit hook (Lefthook)

`project-structure/SKILL.md` covers the pre-commit hook contract and explains why Lefthook is the default. For a FreePascal project on this toolchain, the canonical `lefthook.yml` runs the project's format-check and a build-and-test step on staged sources:

```yaml
pre-commit:
  commands:
    format-check:
      glob: "*.pas"
      run: <project's format-check command, with --check> {staged_files}
    build-check:
      run: <project's build-and-test command>
```

Replace the placeholder commands with the project's actual entry points (whatever satisfies the contracts above). Wire `lefthook install` into `docs/quick-start.md`. Do not skip hooks (`--no-verify`) unless the user explicitly asks. Verify the current Lefthook major version before pinning.

### Repo layout

Defer the universal patterns to `project-structure/SKILL.md` (top-level layout, docs template, AGENTS.md template with "Hard Constraints", agent-file symlinks, multi-area repos, scripts directory, changelog with git-cliff). Stack-specific conventions:

- **Source organization: namespace-based filenames, flat by default.** All Pascal source lives under `source/` (Pascal convention; `src/` is acceptable when the project prefers it — pick one and stay consistent). Units are named `ProjectName.Namespace.UnitName.pas` (the project may use a `ProjectShortName` prefix instead) so the namespace tree is encoded in the filename. The default layout is flat: every unit sits directly under `source/`. Introduce subfolders only when a single namespace has grown enough that flat browsing is hard to manage, and keep the folder name aligned with the namespace it groups. The shared compiler-flag include and toolchain entry points live at the repo root.
- **Build artifacts** land under `build/` and are not committed.
- **Co-located unit tests** alongside the Pascal unit they cover. End-to-end test corpora (when the project has them) live under `tests/`, organized by feature.
- **Scripts: InstantFPC.** One-off scripts are written in Pascal and run via **InstantFPC** (`#!/usr/bin/env instantfpc`) so they execute directly without a separate compile step. Fall back to a `Makefile` / `Justfile` or another dynamic language only when an InstantFPC script would be heavier than the task warrants. See `project-structure/SKILL.md` for the language-of-scripts rule.

### Rules

- **FPC version is verified live.** Before adding or changing any code path that depends on FPC behavior, the agent confirms the version in use (`fpc -iV`) against the pin in `docs/tooling.md` or CI. Memory and prior conversation turns are not acceptable sources.
- **Project-specific tool versions are verified live.** Before touching them, the agent confirms versions of the build system, the formatter / linter, the `git-cliff` pinned by `cliff.toml`, and the Lefthook pinned by `lefthook.yml`.
- **The project's `AGENTS.md` Hard Constraints are read first** — the root file plus the area-specific `AGENTS.md` for the area being touched in a multi-area repo.
- **The project's `docs/build-system.md` (or equivalent) is the source of truth for build entry points and target list.** This skill describes the contract, not the project's specific entry point or target names.
- **The project's verification gate runs clean before handoff.** Both the format-check command and the build-and-test command exit zero. Typical shape:

```bash
<project's format-check command>
<project's build-and-test command>
```

- **Substantive changes update the relevant `docs/` file** per the no-duplication and immutability rules in `project-structure/SKILL.md`.
- **Anything not prescribed by this skill** is governed by the project's own AGENTS.md and `docs/`.

## Examples

**Build system contract — same shape, different implementations:**

| Implementation | Default build | Single target | Prod build | Clean |
| --- | --- | --- | --- | --- |
| Pascal program | `./build` | `./build mytool` | `./build --prod` | `./build clean` |
| Makefile | `make` | `make mytool` | `make MODE=prod` | `make clean` |
| Justfile | `just` | `just build mytool` | `just build --prod` | `just clean` |

Any of these is acceptable as long as the project documents the entry point and the contract holds.

**Pre-commit gate (after substituting placeholders):**

```bash
<project's format-check command>
<project's build-and-test command>
```
