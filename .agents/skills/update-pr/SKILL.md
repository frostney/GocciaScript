---
name: update-pr
description: >-
  Commits relevant local changes, merges origin/main when the branch is behind,
  pushes to the current PR branch, and refreshes PR title/body when stale. Use
  when the user runs /update-pr or asks to update a pull request with the latest
  commits.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access.
---

# Update PR

## Instructions

This workflow is explicit permission to commit relevant changes and push to the current PR branch.

### Rules

- **Never amend commits.** Always create new commits.
- **Never force push.** Use `git push` without `--force` or `--force-with-lease`.

### Steps

1. Inspect repository state:
   - `git status --short --branch`
   - `git diff`
   - `git diff --staged`
   - `git log --oneline -5`
2. If the current branch is `main`, stop and ask for the intended PR branch.
3. Confirm an open PR exists: `gh pr view`. If none, ask whether to run `/create-pr` instead.
4. If the branch is behind `origin/main`, merge baseline:

   ```bash
   git fetch origin main
   git merge origin/main --no-edit
   ```

   Resolve conflicts and commit the merge if needed before continuing.

5. If there is nothing new to commit (aside from an already-finished merge), skip to step 8.
6. Stage only relevant files. Exclude secrets and unrelated local changes.
7. Commit with a concise message via HEREDOC.
8. Push (set upstream if needed):

   ```bash
   git push -u origin HEAD
   ```

9. Reconcile PR title and body with the latest implementation:
   - Read `.github/pull_request_template.md` or `.github/PULL_REQUEST_TEMPLATE/` if present.
   - `gh pr view --json body,url,title`
   - Align title, Summary, Testing, linked issues, and scope with commits and verification.
   - Update title: `gh pr edit --title "$PR_TITLE"` when stale.
   - Update body: `gh pr edit --body-file <file>` when stale. Keep template structure and reviewer context.

10. Report: commit hash, branch, PR URL, whether title/body changed, and verification performed.

Do not skip git hooks or verification unless the user explicitly requests it.
