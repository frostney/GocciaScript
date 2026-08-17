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

When the current PR belongs to a native GitHub stack, read
[../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).

1. Inspect repository state, recent commits, the remote default branch, and the
   current PR.
2. Stop if on the base branch or no open PR exists; report the required next
   workflow.
3. When an ordinary branch is behind the remote base, merge it into the branch.
   For a verified native stack, capture remote heads and use the guarded
   `gh stack sync` or narrower official stack operation; never use raw rebase or
   force-push commands.
4. Run the repository's declared PR gate unless it already passed on the
   unchanged current diff.
5. Stage only relevant files and commit them with a concise Conventional Commit
   subject. Never amend and never skip hooks.
6. Push an ordinary branch normally, setting upstream when needed. Push a
   verified stack only through the guarded official stack workflow.
7. Reconcile the PR title and body with the current commits, scope, linked
   issues, and observed verification. Keep the title a Conventional Commit
   subject for the whole change; the squash merge makes it the commit subject on
   the base branch, so widened scope may also change its type. Preserve the project template or existing
   body structure and omit filler or repeated summaries.
8. Report the commit, branch, PR URL, native stack position when applicable,
   metadata changes, rewritten stack branches if any, and observed validation.
