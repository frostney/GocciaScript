---
name: create-pr
description: >-
  Validates and repairs an in-scope change, publishes its draft pull request,
  reconciles metadata and CI, and marks it ready for review. Use when asked to
  open, raise, or publish a pull request for the current change, or when the
  user runs /create-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git, Python 3.11 or newer, the GitHub CLI (gh) 2.99 or newer
  authenticated to the target repository with push access, the internal
  `delivery-wait` skill, and network access.
---

# Create PR

Publish the current change as a verified, accurately described pull request
that is ready for review. The request authorizes the repository's declared
gates, relevant commits, PR metadata updates, one ordinary draft pull request or
the confirmed native stack layers owned by the change, required review and
behavior testing, in-scope fixes, and transitions to ready for review. This
includes the project's existing PR preview path when required to test the
change. Merge and integration delivery remain with an authorized parent such as
`/deliver`; a standalone publication request does not add those endpoints or
unrelated changes.

A failed check, review or CI run routes to repair, not a stop. Stop only when
there is nothing to publish, a material unresolved choice or new authority is
needed, or an external blocker remains after safe alternatives are exhausted.
A material choice found before publication stops before any push or draft PR.
Keep the PR draft while any required evidence or CI is missing, pending or
failing.

## Conditional references

- When the branch belongs to a native GitHub stack, read
  [../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).
  The request then authorizes submission and metadata reconciliation for the
  confirmed stack layers owned by the current change, not unrelated branches.
- When the change has visible or demonstrable behavior, or local screenshots or
  videos belong in the PR, read
  [references/walkthroughs.md](references/walkthroughs.md). It owns media
  capture, upload and gap reporting. Never commit evidence media to the
  repository. Missing media capabilities alone do not block publication or
  readiness; required behavior evidence and project gates still apply.

## Preview-only draft

When required behavior can only be tested on a PR preview, open or update the
draft needed to obtain it under this publication request after the available
checks pass. Publish a native stack's necessary drafts through the same
permitted stack path. Test that exact revision and resume the loop before
readiness. Recording a walkthrough does not replace behavior testing.

## Workflow

1. Inspect the working tree, staged diff, recent commits, remote default branch,
   stack topology when applicable, and any existing remote head. Preserve
   unrelated local work.
2. Stop if there are no relevant changes or commits ahead of the remote base.
   Continue without an empty commit when the work is already committed.
3. If currently on the base branch, create a focused branch named from the issue
   or change.
4. Establish the PR claim and publication requirements before publishing
   anything. Read the explicit request, linked issue or confirmed mini-spec,
   applicable project instructions, product docs, ADRs or durable decisions,
   the nearest `DEFINITION_OF_READY.md`, and the completion evidence supplied by
   the implementation workflow. If no source states the claim, reconstruct the
   narrowest claim supported by the commits and diff and label it as inferred.
5. Apply `/code-review fix-all` and `/test-against-spec fix` to the current
   requirements and change. Reuse only results whose content, command,
   environment and coverage still match; run missing or stale review and
   real-interface checks. If the claim was inferred from the diff,
   establish explicit expected behavior before specification testing. Resolve a
   material ambiguity without inventing requirements from the implementation.
6. Repair every verified in-scope requirement, fidelity, test and compatibility
   gap, regardless of severity, through `/implement`'s development loop; use the
   same documented checks directly if a companion skill is unavailable. After
   edits, obtain renewed review and specification results for the affected
   scope before publication or readiness. Targeted finding revalidation can
   reuse unchanged review coverage; fixing a finding does not itself renew the
   review result. Run the missing or invalidated project gate after fixes
   converge.
7. Commit uncommitted relevant work under `git-workflow`: stage only relevant
   files, excluding secrets and unrelated local work, and use a concise
   Conventional Commit subject. Never amend and never skip hooks. Preserve
   already-published history and add a new commit for any correction.
8. Title each pull request with a Conventional Commit subject covering the whole
   change; `git-workflow`'s squash merge makes that title the base-branch commit
   subject. Follow
   [../agent-writing/references/pr-descriptions.md](../agent-writing/references/pr-descriptions.md)
   for the body, preserving explicit project-template requirements. Before
   writing it, search open and closed issues and recent sibling sessions or
   adjacent branches when available for related findings and duplicates. Put
   each closing keyword on its own line as `Closes #N`, and only on the layer
   that completes that issue.
9. After the publication checks pass, or under the preview-only draft rule
   above, push an ordinary branch normally and set its upstream when needed,
   then open one draft PR against the remote default.
   For a verified native stack, follow the stack reference: a new top layer
   above frozen approved PRs uses protected push, separate PR creation and
   native append; broader authorized submissions use `gh stack submit`. Require
   current evidence for every published layer. Only guarded official stack
   operations may rebase or push with force-with-lease. Preserve bottom-to-top
   topology and keep each layer draft.
10. Run the PR-specific phase. Compare the actual PR diff, body, links, metadata,
    committed tests and documentation, supplied completion evidence, and facts
    that exist only after publication. Correct metadata-only gaps without a
    commit.
11. If the PR-specific phase or CI exposes an in-scope repository, behavior,
    fidelity, test, documentation or compatibility gap, repair it through the
    development loop. Revalidate affected requirements, then use `/update-pr`
    for the same PR and verify its new head. Continue until all verified
    requirement gaps are resolved.
12. Invoke `delivery-wait`'s foreground `wait checks-terminal` operation with
    the repository, PR number, exact head, required `--check` contexts, absolute
    deadline and `--json`. Passively await meaningful transitions; do not
    substitute repeated model turns for a passive wait, and report an
    unavailable host capability. Inspect failed logs and return established
    in-scope causes to step 11. If expected PR checks have no run, inspect
    mergeability and resolve an established conflict through `/update-pr`
    before considering a retrigger. An unavailable external dependency remains
    pending or blocked, never a passed check.
13. Once the PR is missing nothing required by the publication and PR-specific
    phases and all applicable CI is observed green for its exact head, mark it
    ready for review. Return every affected URL, native stack order when
    applicable, final states, metadata changes, supplied completion evidence,
    and observed readiness and CI evidence. Include incomplete walkthrough
    requirements and actionable remedies, even when the PR is ready.
