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

## Instructions

This workflow is explicit permission to commit relevant changes and push to the current PR branch.

### Rules

- **Never amend commits.** Always create new commits.
- **Never force push.** Use `git push` without `--force` or `--force-with-lease`.

### Steps

1. Inspect repository state and resolve the base branch (never hardcode `main`):
   - `git status --short --branch`
   - `git diff`
   - `git diff --staged`
   - `git log --oneline -5`
   - `BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')`
2. If the current branch is the base branch, stop and ask for the intended PR branch.
3. Confirm an open PR exists: `gh pr view`. If none, ask whether to run `/create-pr` instead.
4. If the branch is behind `origin/$BASE_BRANCH`, merge baseline:

   ```bash
   git fetch origin "$BASE_BRANCH"
   git merge "origin/$BASE_BRANCH" --no-edit
   ```

   Resolve conflicts and commit the merge if needed before continuing.

5. If there is nothing new to commit (aside from an already-finished merge), skip to step 8.
6. Stage only relevant files. Exclude secrets and unrelated local changes.
7. Commit with a concise Conventional Commit message via HEREDOC:
   - Subject format: `type(scope): summary`.
   - Use one of: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`, `revert`.
   - Pick the narrowest accurate scope; omit the scope only when no meaningful scope exists.
   - Use imperative mood, lowercase the summary after the type/scope, and do not end the subject with a period.
8. Push (set upstream if needed):

   ```bash
   git push -u origin HEAD
   ```

9. Reconcile PR title and body with the latest implementation:
   - Search `.github/pull_request_template.md` and `.github/PULL_REQUEST_TEMPLATE/`; read every matching PR template relevant to the current PR.
   - Absence protocol: after the template search finds no PR template, state that no project PR template was found and reconcile against the existing PR body structure.
   - `gh pr view --json body,url,title`
   - Align title, Summary, Testing, linked issues, and scope with commits and verification.
   - Update title: `gh pr edit --title "$PR_TITLE"` when stale.
   - Update body: `gh pr edit --body-file <file>` when stale. Keep template structure and reviewer context.

10. Report: commit hash, branch, PR URL, whether title/body changed, and verification performed.

Do not skip git hooks or verification unless the user explicitly requests it.
