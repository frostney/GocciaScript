---
name: create-pr
description: >-
  Commits relevant changes, opens a templated draft pull request, fills gaps
  against the repository's Definition of Ready, fixes CI, and marks it ready.
  Use when the user runs /create-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git, Python 3.11 or newer, the GitHub CLI (gh) authenticated to the
  target repository, the internal `delivery-wait` skill, and network access.
---

# Create PR

The request authorizes the repository's declared gates, relevant fixes and
commits, PR metadata updates, one ordinary draft pull request or the confirmed
native stack layers owned by the change, and their transitions to ready for
review. It does not authorize unrelated changes.

When the branch belongs to a native GitHub stack, read
[../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).
The request then authorizes submission and metadata reconciliation for the
confirmed stack layers owned by the current change, not unrelated branches.

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
7. For an ordinary branch, push normally and set its upstream when needed. For
   a verified native stack, use `gh stack submit`; only its guarded official
   stack operations may rebase or push with force-with-lease.
8. Title each pull request with a Conventional Commit subject covering the whole
   change, since the squash merge makes that title the commit subject on the
   base branch. Fill the matching PR template for each submitted layer,
   preserving its structure. If none exists, use
   Summary, Testing, and Linked issues. Keep the body proportional to the change.
   Put each closing keyword on its own line: `Closes #N`, and only on the layer
   that completes that issue.
9. Open one draft PR against the remote default for an ordinary branch. For a
   stack, preserve bottom-to-top native topology and keep each new layer draft
   until that layer satisfies the remaining gates.
10. Find the nearest applicable `DEFINITION_OF_READY.md`. If it exists, compare
    every criterion with the actual PR diff, tests, documentation, linked work,
    metadata, and validation evidence to identify anything the PR is missing. If
    absent after a real search, use this workflow's built-in gates.
11. Fill every in-scope readiness gap. Add missing implementation, tests,
    documentation, or artifacts, or correct the PR body and links. For
    repository changes, validate, create a new commit, and push normally;
    metadata-only fixes require no commit. Never mark a criterion satisfied
    without observed evidence.
12. Invoke `delivery-wait`'s foreground `wait checks-terminal` operation with the
    repository, exact head, checkpoint, absolute deadline, and `--json`; the
    harness passively awaits it without model heartbeats. When it returns, inspect
    any unsuccessful logs, fix the in-scope root cause without weakening the
    gate, run the relevant local checks, create a new commit, push normally, and
    wait on the new exact head. If the host cannot passively await a subprocess,
    report the unsupported capability instead of polling through model turns.
13. Keep the PR in draft while any readiness criterion or CI check is pending or
    failing. Continue monitoring pending checks; if they cannot reach a terminal
    result during the run, report their current state. Stop and report the exact
    blocker when satisfying it requires a material product decision, unrelated
    work, unavailable external service, or a change that cannot be validated
    safely.
14. Once a PR is missing nothing required by the Definition of Ready and all
    applicable CI is observed green for its exact head, mark that PR ready for
    review. Return every affected URL, native stack order when applicable,
    final states, fixes made, and observed readiness and CI evidence.
