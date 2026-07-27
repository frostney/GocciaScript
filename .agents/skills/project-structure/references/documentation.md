# Documentation and repository templates

## README

The root README is for users, not a contributor manual. Use this order, omitting
optional sections rather than inventing content:

1. `# Project Name`
2. Logo, only when an asset exists
3. Description, at most 350 characters
4. Canonical install line, at most 100 characters
5. One to three primary usage flows, with links to deeper docs
6. Optional background
7. Contribution summary, at most 150 characters, with a detailed link
8. Links to `AGENTS.md` and the license

Deep build, test, and contributor guidance belongs in `docs/` or
`CONTRIBUTING.md`. README links are subject to the normal lint and link gates.

## Docs

Use the following files when the topics apply:

| File | Owns |
| --- | --- |
| `architecture.md` or `application.md` | Stack, concepts, data model, boundaries |
| `quick-start.md` | Zero to running |
| `tooling.md` | Commands, environment, lint, format, test |
| `code-style.md` | Naming, layout, design and dependency rules |
| `deployment.md` | Build profiles, CI/CD, release, rollback |
| `testing.md` or `testing-pattern.md` | Non-obvious test policy |
| `decision-log.md` | Append-only decisions |
| `spikes/` | Immutable point-in-time investigations |

Every docs file except the decision log and root entry points starts with an
`## Executive Summary` containing three to six bullets. Each topic has one
authoritative home; other documents link to it. Do not rewrite old decision-log
entries or spikes to match later reality.

## Governance content

- `VISION.md`: purpose, audience, durable technical direction, and explicit
  non-goals.
- `DEFINITION_OF_READY.md`: outcome, scope, acceptance criteria, constraints,
  and resolved dependencies.
- `DEFINITION_OF_DONE.md`: implementation, meaningful tests, current docs,
  passing project gate, review evidence, and required handoff artifacts.

Every applicable readiness or completion item is a gate, not a suggestion.

## Multi-area repositories

- Root README and AGENTS cover the whole repo and point to each area.
- Add an area README and AGENTS only when that area has distinct context or
  rules.
- Put area docs under `docs/<area>/` or inside the area; choose one convention.
- State in root AGENTS that changes must be scoped to the correct area.
