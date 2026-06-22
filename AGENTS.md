# AGENTS.md

Instructions for **AI coding assistants** (Cursor, Claude Code, and similar) working in this repository.

## Contributing guide vs this file

| | Purpose |
|---|--------|
| **[CONTRIBUTING.md](CONTRIBUTING.md)** | **Contributing guide for all contributors**—humans and agents. Workflow, critical rules, testing, Pascal code style, formatting, build/run reference, documentation index. If it affects what may be merged, it belongs there. |
| **This file (`AGENTS.md` / `CLAUDE.md`)** | **Agent-only** context: how assistants should operate in this repo, where to read first, and what not to duplicate. It does **not** replace CONTRIBUTING. |

Assistants should treat CONTRIBUTING as authoritative for contribution requirements. Use this file for assistant-specific expectations and navigation, not a second copy of CONTRIBUTING.

## Expectations for assistants

- **Read [CONTRIBUTING.md](CONTRIBUTING.md)** before substantive edits—especially [Critical rules](CONTRIBUTING.md#critical-rules) and [Code style](docs/contributing/code-style.md).
- **Run verification yourself** when the environment allows (tests, format check); do not only tell the human what to run unless execution is impossible.
- **Match the project's workflow**: branch from `main`, focused diffs, tests and docs updated per CONTRIBUTING.
- **Treat project-local skills as external playbooks**: files under `.agents/skills/` are not normal repo documentation. Do not edit them unless the user explicitly asks to change that skill. Put repo-specific assistant expectations in this file, or create/update a separate skill only when explicitly requested.
- **Infer architecture boundaries during planning**: when a change touches website routes, API handlers, generated reports, external services, credentials, artifacts, caches, CI outputs, or deployment/build steps, identify where the work belongs (build time, request time, client time, CI/scheduled time) and compare against existing project patterns before implementing. Do not rely on the user or a skill checklist to spell this out.
- **Clean first for stale FPC failures**: after a merge, branch switch, PR sync,
  generated resource change, or unexplained compiler/resource error, retry with
  `./build.pas --clean <target>` (or `./build.pas --clean`) before diagnosing the
  reported source line. See [Tooling — Stale FPC Build Artifacts](docs/contributing/tooling.md#stale-fpc-build-artifacts).
- **Do not paste large chunks of CONTRIBUTING into this file** when CONTRIBUTING changes—edit CONTRIBUTING instead, and keep AGENTS short.

## TC39 spec lookup

For ECMAScript or ECMA-402 behavior, use the project TC39 MCP server before falling back to web sources or large spec HTML files. The canonical project MCP config lives under `.agents/mcp/`, with client-facing links/adapters for supported tools. The shared JSON shape is:

```json
{
  "mcpServers": {
    "tc39": {
      "command": "npx",
      "args": ["-y", "tc39-mcp@0.4.0"]
    }
  }
}
```

Use `spec.search` when you do not know the clause id, `clause.get` when you do, `spec.crossrefs` to follow abstract-operation dependencies, `spec.diff` / `spec.history` for prose drift, `test262.search` and `test262.get` to map clauses to conformance tests, and `proposal.list` / `proposal.get` for proposal-stage features. When making durable claims in code review, issues, or PR notes, record the spec, edition, clause id, section number, and snapshot SHA when the MCP response provides one. If `tc39-mcp` is unavailable, fall back to the official TC39 sources (`tc39.es/ecma262`, `tc39.es/ecma402`) rather than guessing.

## Quick checks

```bash
./build.pas testrunner && ./build/GocciaTestRunner tests && ./build/GocciaTestRunner tests --mode=bytecode  # after substantive changes
./build.pas bundler && ./build/GocciaBundler example.js  # build and run the bundler
./format.pas --check # before push / PR
```

## Quick reference

The authoritative command reference lives in [Build System](docs/build-system.md).
Use that file for build targets, CLI options, configuration-file behavior,
`GocciaScriptLoader`, `GocciaTestRunner`, `GocciaBenchmarkRunner`, and
`GocciaBundler` examples. Keep this file agent-only; do not duplicate build or
runtime command lists here.

## Where to go next

- **Contribution requirements:** [CONTRIBUTING.md](CONTRIBUTING.md)
- **Engine shape:** [docs/architecture.md](docs/architecture.md), [docs/interpreter.md](docs/interpreter.md), [docs/bytecode-vm.md](docs/bytecode-vm.md), [docs/core-patterns.md](docs/core-patterns.md)
- **Optional extended agent skills:** [.agents/skills/](.agents/skills/) (installable playbooks; not a substitute for CONTRIBUTING)
