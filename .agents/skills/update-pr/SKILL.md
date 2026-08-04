---
name: update-pr
description: >-
  Commits relevant changes, merges the remote default when needed, pushes the
  current pull-request branch, and refreshes stale PR metadata. Use when the
  user runs /update-pr or asks to update a pull request.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access.
---

# Update PR

The request authorizes the repository's declared PR gate, relevant commits, a
plain push to the current PR branch, and metadata reconciliation. It does not
authorize unrelated changes.

1. Inspect repository state, recent commits, the remote default branch, and the
   current PR.
2. Stop if on the base branch or no open PR exists; report the required next
   workflow.
3. When behind the remote base, merge it into the branch. Never rebase.
4. Run the repository's declared PR gate unless it already passed on the
   unchanged current diff.
5. Stage only relevant files and commit them with a concise Conventional Commit
   subject. Never amend and never skip hooks.
6. Push normally, setting upstream when needed. Never force-push.
7. Reconcile the PR title and body with the current commits, scope, linked
   issues, and observed verification. Preserve the project template or existing
   body structure and omit filler or repeated summaries.
8. Report the commit, branch, PR URL, metadata changes, and observed validation.
