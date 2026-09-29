# AGENTS.md

Instructions for **AI coding assistants** (Cursor, Claude Code, and similar) working in this repository.

## Contributing guide vs this file

| | Purpose |
|---|--------|
| **[CONTRIBUTING.md](CONTRIBUTING.md)** | **Contributing guide for all contributors**—humans and agents. Workflow, critical rules, testing, Pascal code style, formatting, build/run reference, documentation index. If it affects what may be merged, it belongs there. |
| **This file (`AGENTS.md` / `CLAUDE.md`)** | **Agent-only** context: how assistants should operate in this repo, where to read first, and what not to duplicate. It does **not** replace CONTRIBUTING. |

Assistants should treat CONTRIBUTING as authoritative for contribution requirements. Use this file for assistant-specific expectations and navigation, not a second copy of CONTRIBUTING.

## Expectations for assistants

- **Follow the [known-good-route operating loop](https://github.com/frostney/known-good-route#operating-loop)** for repository work. A defect in its skills or helpers is fixed upstream in known-good-route, not worked around here.
- **Vendored vs repo-native skills**: skills listed in `skills-lock.json` are vendored by the skills CLI. Never hand-edit them; fix them upstream (known-good-route for its skills). Edit a repo-native skill (`prepare-release`, `gocciascript-issue-validation`, `optimize-runtime`, `profile-report-review`) only when the user asks to change that skill.
- **Infer architecture boundaries during planning**: when a change touches website routes, API handlers, generated reports, external services, credentials, artifacts, caches, CI outputs, or deployment/build steps, identify where the work belongs (build time, request time, client time, CI/scheduled time) and compare against existing project patterns before implementing. Do not rely on the user or a skill checklist to spell this out.
- **Search the real source layout**: when prompts, automations, or audits search this repo, target `source/units`, `source/shared`, `source/app`, `tests`, `scripts`, and `website/src` explicitly. Do not assume a generic root `src/` tree; `source/generated` is generated data and should only be inspected or regenerated when the task specifically requires it.
- **Do source deep-dives before policy claims**: follow the [investigation discipline](.agents/skills/software-engineering-excellence/references/investigation.md). For ECMAScript the normative source is ECMA-262/ECMA-402 (see [TC39 spec lookup](#tc39-spec-lookup)); classify each surface as shim-, parser-, runtime-, or object-model-level by the mechanism it needs.
- **Clean first for stale FPC failures**: after a merge, branch switch, PR sync,
  generated resource change, or unexplained compiler/resource error, retry with
  `./build.pas --clean <target>` (or `./build.pas --clean`) before diagnosing the
  reported source line. See [Tooling — Stale FPC Build Artifacts](docs/contributing/tooling.md#stale-fpc-build-artifacts).

## TC39 spec lookup

For ECMAScript or ECMA-402 behavior, use the project TC39 MCP server before falling back to web sources or large spec HTML files. The canonical project MCP config lives under `.agents/mcp/`, with client-facing links/adapters for supported tools. The shared JSON shape is:

```json
{
  "mcpServers": {
    "tc39": {
      "command": "npx",
      "args": ["-y", "tc39-mcp@0.6.3"]
    }
  }
}
```

Use `spec.search` when you do not know the clause id, `clause.get` when you do, `spec.crossrefs` to follow abstract-operation dependencies, `spec.diff` / `spec.history` for prose drift, `test262.search` and `test262.get` to map clauses to conformance tests, and `proposal.list` / `proposal.get` for proposal-stage features. When making durable claims in code review, issues, or PR notes, record the spec, edition, clause id, section number, and snapshot SHA when the MCP response provides one. If `tc39-mcp` is unavailable, fall back to the official TC39 sources (`tc39.es/ecma262`, `tc39.es/ecma402`) rather than guessing.

## Quick checks

```bash
./build.pas testrunner && ./build/GocciaTestRunner -P tests && ./build/GocciaTestRunner -P tests --mode=bytecode  # after substantive changes
./build.pas bundler && ./build/GocciaBundler example.js  # build and run the bundler
./format.pas --check # before push / PR
```

Assistants pass `-P` and never run `--trust`; [Testing requirements](CONTRIBUTING.md#3-testing-requirements) explains why.

## Quick reference

The authoritative command reference lives in [Build System](docs/build-system.md).
Use that file for build targets, CLI options, configuration-file behavior,
`GocciaRunner`, `GocciaTestRunner`, `GocciaBenchmarkRunner`, and
`GocciaBundler` examples. Keep this file agent-only; do not duplicate build or
runtime command lists here.

## Where to go next

- **Engine shape:** [docs/architecture.md](docs/architecture.md), [docs/interpreter.md](docs/interpreter.md), [docs/bytecode-vm.md](docs/bytecode-vm.md), [docs/core-patterns.md](docs/core-patterns.md)
- **Optional extended agent skills:** [.agents/skills/](.agents/skills/) (installable playbooks; not a substitute for CONTRIBUTING)
- **Runtime optimization waves:** [.agents/skills/optimize-runtime/SKILL.md](.agents/skills/optimize-runtime/SKILL.md) — Use when closing the bytecode-vs-QuickJS gap or running a measured runtime optimization wave. Benchmark-gated; keep only measured wins
