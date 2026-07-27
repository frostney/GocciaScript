---
name: create-pr
description: >-
  Commits relevant changes, opens a templated draft pull request, fills gaps
  against the repository's Definition of Ready, fixes CI, and marks it ready.
  Use when the user runs /create-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access.
---

# Create PR

## Instructions

The request authorizes the repository's declared gates, relevant fixes and
commits, plain pushes, PR metadata updates, one draft pull request, and its
transition to ready for review. It does not authorize unrelated changes.

1. Inspect the working tree, staged diff, recent commits, and remote default
   branch.
2. Stop if there are no relevant changes or commits ahead of the remote base.
   Continue without an empty commit when the work is already committed.
3. If currently on the base branch, create a focused branch named from the issue
   or change.
4. Run the repository's declared pre-PR gate unless it already passed on the
   unchanged current diff. Never claim an unobserved result.
5. Stage only relevant files, excluding secrets and unrelated local work.
6. Commit uncommitted work with a concise Conventional Commit subject. Never
   amend and never skip hooks.
7. Push normally, setting upstream when needed. Never force-push.
8. Fill the matching PR template, preserving its structure. If none exists, use
   Summary, Testing, and Linked issues. Keep the body proportional to the change.
   Put each closing keyword on its own line: `Closes #N`.
9. Open one draft PR against the remote default branch.
10. Find the nearest applicable `DEFINITION_OF_READY.md`. If it exists, compare
    every criterion with the actual PR diff, tests, documentation, linked work,
    metadata, and validation evidence to identify anything the PR is missing. If
    absent after a real search, use this workflow's built-in gates.
11. Fill every in-scope readiness gap. Add missing implementation, tests,
    documentation, or artifacts, or correct the PR body and links. For
    repository changes, validate, create a new commit, and push normally;
    metadata-only fixes require no commit. Never mark a criterion satisfied
    without observed evidence.
12. Monitor every applicable CI check to a terminal result. While a check is not
    green, inspect its logs, fix the in-scope root cause without weakening the
    gate, run the relevant local checks, create a new commit, push normally, and
    monitor the new run.
13. Keep the PR in draft while any readiness criterion or CI check is pending or
    failing. Continue monitoring pending checks; if they cannot reach a terminal
    result during the run, report their current state. Stop and report the exact
    blocker when satisfying it requires a material product decision, unrelated
    work, unavailable external service, or a change that cannot be validated
    safely.
14. Once the PR is missing nothing required by the Definition of Ready and all
    applicable CI is observed green, mark it ready for review. Return its URL,
    final state, fixes made, and observed readiness and CI evidence.
