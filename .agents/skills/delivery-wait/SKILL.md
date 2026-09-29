---
name: delivery-wait
description: >-
  Provides deterministic, resumable GitHub transition waits used internally by
  delivery workflows. Use when another workflow must await CI, merge, tag, or
  release state without model heartbeats.
license: Unlicense OR MIT
compatibility: >-
  Requires Python 3.11 or newer, the GitHub CLI (gh) authenticated to the target
  repository, and network access.
---

# Delivery wait

Run `scripts/delivery_wait.py` as a silent foreground child whenever a workflow
must await external GitHub state. The harness passively waits for the command to
finish; it must not simulate waiting with model heartbeats.

The caller supplies the repository, expected identity, absolute deadline, and
an optional state path. The command reconciles any saved state with fresh
GitHub state, waits while nothing relevant changes, and exits with one result.
Use `--json` from workflow skills; human output is for direct terminal use.
Skills default state files beneath `.agent/waits/`, which the consuming
repository must gitignore; another harness may supply any private state path
through `--state`.

## Invocation

`wait <kind> --repo <owner/repo> --deadline <ISO 8601> [--interval <seconds>]
[--state <path>] [--json]` runs a foreground wait; `inspect <kind>` takes the
same identity flags without `--deadline`, `--interval`, or `--state`. Each kind
requires its own identity flags:

- `checks-terminal`: `--pr <number>`, `--head <sha>`, and `--all-workflows`,
  one `--check <name>` per expected check context, or both. A run with neither
  exits with an operational error instead of waiting.
  - `--all-workflows` needs no check names. It finds every GitHub Actions
    workflow run for the exact head and every job in each run's latest attempt.
    It waits while there is no run, or while any run or job is queued, in
    progress, waiting, pending or requested. It reports `changed` as soon as a
    run or job ends in failure, cancelled, timed out, action required or
    another non-success conclusion. It reports `satisfied` only when every run
    and job succeeded, was skipped or was neutral, and two consecutive
    observations one interval apart show the same census. Jobs added by later
    matrix or `needs` stages keep their run in progress. A run started after
    another run finishes shows up in the second observation. `inspect` cannot
    confirm a stable census, so it reports `waiting` instead of `satisfied` for
    an all-success snapshot.
  - `--all-workflows` does not cover commit statuses or check runs from other
    apps, such as CodeRabbit or Vercel. Add a `--check` for each one the caller
    needs; the wait then also requires those contexts to succeed.
  - `--check` alone judges only the named contexts, whatever else is still
    running for the head. Use it only when the complete expected set is known,
    such as the repository's required status checks. Checks visible at wait
    time are not that set: workflows add jobs in stages.
- `pr-merged`: `--pr <number>` and `--head <sha>`.
- `workflow-terminal`: `--run-id <id>` and `--head <sha>`. Use it for a
  dispatched or other run whose checks do not appear on the pull request.
- `tag-target`: `--tag <name>` and `--head <sha>`.
- `release-assets`: `--tag <name>`, `--head <sha>`, and one `--asset <name>` per
  required asset.
- `wake-at` (`wait` only): `--deadline` alone. It keeps no state file, so
  `--state` has no effect.

There is no `--checkpoint` flag. The state path is `--state`, and the command
derives a default path from the kind and identity when it is omitted.

## Results

`--json` prints one envelope whose `state` is the result. Only `satisfied`
establishes the awaited fact. `changed` means a check or workflow reached a
non-success terminal result or other relevant state moved; inspect it before
deciding. `invalidated` means the expected head, ref, or saved identity no
longer matches, `timed-out` means the deadline passed, and `cancelled` means
the wait was interrupted. These exit 0; `operational-error` exits 2. `inspect`
can also report `waiting`.

Use `inspect` for a single authoritative delivery snapshot and `wait` for a
foreground transition wait. Supported predicates are exact-head checks,
pull-request merge, workflow completion, tag target, release assets, and an
absolute wake time. A changed expected head or ref invalidates the wait instead
of being followed silently. GitHub remains authoritative; checkpoints contain
normalized identities and observations, no credentials, raw comment bodies, or
raw API payloads.

The helper uses authenticated `gh` transport. GraphQL is primary. A REST
fallback is used only when GraphQL is rate-limited and REST preserves the same
fact, or when GitHub exposes the fact only through REST.
