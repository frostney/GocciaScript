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

Apply only the contract material to the requested work. Project AGENTS and docs
own implementation rules and exact commands; `project-structure/SKILL.md` owns
language-neutral layout and documentation.

## Compiler and layout

- Use the project-pinned latest stable FPC and Delphi mode unless project docs
  explicitly choose another mode.
- Keep project-wide compiler directives in one shared include used by every
  unit.
- Put Pascal source under one root (`source/` preferred), using namespaced
  filenames and a flat layout until a namespace becomes unwieldy.
- Co-locate unit tests; keep delivered end-to-end corpora under root `tests/`.
- Put all ignored build output under root `build/`.
- Prefer executable InstantFPC for one-off Pascal scripts; use an existing task
  runner or dynamic language only when that is materially simpler.

Read [references/code-style.md](references/code-style.md) when writing or
reviewing Pascal source.

## Build contract

The project chooses the implementation but provides one root entry point:

- bare invocation performs a clean development build of all binaries;
- positional names select one or more binary targets;
- development is the default and a production flag selects release settings;
- `clean` is a target and can be combined with a target;
- every binary lands under `build/`.

Document the entry point, targets, and flags in `docs/build-system.md`; read it
before changing them.

## Formatter and health contracts

One formatter/linter command rewrites Pascal by default and has a non-mutating
`--check` mode that exits non-zero on deviation. Project-specific style lives in
the tool and `docs/code-style.md`, not scattered config.

The codebase-health gate reports located duplication, per-function cyclomatic
and cognitive complexity, sortable file health, and complexity-times-churn
hotspots. Thresholds and ignores live in one root config; breaches exit
non-zero. Document the tool and thresholds in `docs/tooling.md`.

## Hook and completion

Lefthook runs the project's formatter check on staged Pascal and its
build-and-test command. Use the exact project entry points, install the hook via
the documented quick start, and never bypass it without explicit user direction.

Before relying on tool behavior, inspect root and area AGENTS, confirm `fpc -iV`
against project/CI pins, and verify any touched tool version live. Before
handoff, observe the project formatter and build-and-test gates passing and
update only the authoritative docs affected by the change.
