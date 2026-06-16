# Definition of Done

Use this gate before handoff, review, or PR creation. A change is not done until every applicable item is satisfied or explicitly marked out of scope by the user.

## Implementation

- The delivered behavior matches the issue, confirmed mini-spec, or requested documentation change without hidden scope changes.
- The change satisfies [CONTRIBUTING.md](CONTRIBUTING.md), including [Implementation principles](CONTRIBUTING.md#implementation-principles) and [Critical rules](CONTRIBUTING.md#critical-rules).
- The change follows applicable project skills under `.agents/skills/`, not only implementation workflow skills.
- Architecture and terminology match [Architecture](docs/architecture.md), [Core patterns](docs/core-patterns.md), [Code Style](docs/contributing/code-style.md), and [CONTEXT.md](CONTEXT.md).

## Tests and Verification

- Test coverage follows [Testing](docs/testing.md) and the public-surface testing strategy identified during readiness.
- The relevant focused checks pass before broader checks.
- **JavaScript suite gate** — Always run `./build.pas testrunner`, `./build/GocciaTestRunner tests`, and `./build/GocciaTestRunner tests --mode=bytecode`.
- **Format gate** — Run `./format.pas --check` before push or PR.
- If a clean build is needed to rule out stale FPC artifacts, the relevant `./build.pas --clean <target>` check has been run before diagnosing source failures.

## Documentation and Decisions

- Documentation updates follow [CONTRIBUTING.md#documentation](CONTRIBUTING.md#documentation).
- Durable architecture or design decisions are recorded as ADRs under `docs/adr/`.
- Existing ADRs remain immutable except for link maintenance.

## Handoff

- The diff has been self-reviewed against the issue or mini-spec criterion by criterion.
- The changeset has been code reviewed by a separate review pass from the implementation work, using `/review` when available or a documented manual diff review when it is not.
- Any reviewer-facing context is captured in the PR body: summary, constraints, tests run, docs updated, ADR links, and any intentionally deferred work.
- There are no unrelated changes mixed into the handoff unless the user explicitly requested them.
