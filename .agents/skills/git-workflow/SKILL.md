---
name: git-workflow
description: >-
  Default git workflow for the user's repositories: feature branches off the
  remote default branch, merge (never rebase) to take baseline updates, plain
  pushes (no --force or --force-with-lease), always-new commits (no amend), and
  squash-merge for all pull requests. Use when running any git action —
  branching, syncing, conflict resolution, committing, pushing, or merging — on
  the user's repos.
license: Unlicense OR MIT
compatibility: >-
  Requires git; pull-request operations also require the GitHub CLI (gh) and
  network access.
---

# Git workflow

## Instructions

These are the user's repository defaults. Apply them on every git action unless the user explicitly overrides them in the same turn.

### Rules

- **Squash-merge** every pull request. Never use "Create a merge commit" or "Rebase and merge" on GitHub.
- **Never rebase.** Use merge to integrate changes — including baseline catch-up and conflict resolution.
- **Never force push.** Plain `git push` only. No `--force`, no `--force-with-lease`.
- **Never amend commits.** Always create new commits. `git commit --amend` is forbidden, even for typo fixes, unless the user explicitly asks in the same turn.

### Branching

Resolve the base branch from the remote default — do not hardcode `main`:

```bash
BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')
```

- Create focused branches off the base. Name them from the issue or change (e.g. `issue-123-short-slug`, `fix-checkout-validation`).
- Never commit directly to the base branch.

### Keeping a branch up to date

When the branch is behind the remote base, **merge** the baseline:

```bash
git fetch origin "$BASE_BRANCH"
git merge "origin/$BASE_BRANCH" --no-edit
```

Resolve any conflicts and commit the merge before continuing. Do not `git rebase origin/$BASE_BRANCH`.

### Commits

- Each logical change is its own commit. Use a HEREDOC for multi-line messages so formatting is preserved.
- Do not amend. If a commit needs a fix-up, add a new commit.
- Do not skip hooks (`--no-verify`) unless the user explicitly asks.

### Pushing

```bash
git push                       # routine push
git push -u origin HEAD        # first push of a new branch
```

Never `git push --force` or `git push --force-with-lease`. If a remote push is rejected because the histories diverged, stop and ask the user — do not paper over with a force push.

### Merging pull requests

- Always **squash-merge** on GitHub. Edit the squash commit message to a clean summary before confirming the merge.
- Delete the source branch after the squash-merge (the GitHub option, or local cleanup).
- After the squash-merge, sync any local working copy that still has the merged branch:

```bash
git checkout "$BASE_BRANCH"
git pull origin "$BASE_BRANCH"
git branch -D <merged-branch>
```

### Exceptions

Deviate from any rule only when the user explicitly asks in the same turn. State the deviation in chat so it is not silently normalized.
