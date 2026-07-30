---
name: git-workflow
description: >-
  Applies the user's git defaults: branch from the remote default, merge rather
  than rebase, never amend or force-push, and squash-merge pull requests. Use
  when branching, syncing, committing, pushing, or merging in the user's repos.
license: Unlicense OR MIT
compatibility: >-
  Requires git; pull-request operations also require the GitHub CLI (gh) and
  network access.
---

# Git workflow

## Instructions

Apply these defaults unless the user explicitly overrides them in the same
turn. A git request authorizes only the repository and forge state required for
that operation.

- Resolve the base from the remote default; never hardcode `main`.
- Create focused branches off the current remote base.
- Merge the remote base to update a branch. Never rebase.
- Never amend commits. Add a new commit for every correction.
- Never force-push. Stop if a plain push is rejected by divergent history.
- Stage only relevant files and exclude secrets or unrelated local work.
- Use concise Conventional Commit subjects in imperative mood.
- Let hooks run unless the user explicitly asks otherwise.
- Squash-merge pull requests and delete the source branch afterward.

After a squash merge, sync the local base and remove the merged local branch.
