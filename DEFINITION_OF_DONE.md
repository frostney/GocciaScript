# Definition of Done

Use this gate before handoff, review, or PR creation. A change is not done until every applicable item is satisfied or explicitly marked out of scope by the user.

## Implementation

- The delivered behavior matches the issue, confirmed mini-spec, or requested documentation change without hidden scope changes.
- The change satisfies [CONTRIBUTING.md](CONTRIBUTING.md), including [Implementation principles](CONTRIBUTING.md#implementation-principles) and [Critical rules](CONTRIBUTING.md#critical-rules).
- The change follows applicable project skills under `.agents/skills/`, not only implementation workflow skills.
- Architecture and terminology match [Architecture](docs/architecture.md), [Core patterns](docs/core-patterns.md), [Code Style](docs/contributing/code-style.md), and [CONTEXT.md](CONTEXT.md).

## Tests and Verification

- Test coverage follows [Testing](docs/testing.md) and the public-surface testing strategy identified during readiness.
- **JavaScript suite gate** — Always run `./build.pas testrunner` and `./build/GocciaTestRunner -P tests`. [Testing requirements](CONTRIBUTING.md#3-testing-requirements) explains `-P` and the `--trust` alternative.
- **Format gate** — Run `./format.pas --check` before push or PR.
- **Platform gate** — A platform-sensitive change has a green `ci.yml` run dispatched for its current head (see [Workflow — Full CI](docs/contributing/workflow.md#full-ci)); PR CI covers Linux x64 only.
- `build.pas` retries a target once from a clean unit directory when FPC reports stale compiled state; a build failure that survives that retry is diagnosed as a source failure, with `./build.pas --clean <target>` run only if it still fails (see [Tooling — Stale FPC Build Artifacts](docs/contributing/tooling.md#stale-fpc-build-artifacts)).

## Documentation and Decisions

- Documentation updates follow [CONTRIBUTING.md#documentation](CONTRIBUTING.md#documentation).
- Durable architecture or design decisions are recorded as ADRs under `docs/adr/`.
- Existing ADRs remain immutable except for link maintenance.
