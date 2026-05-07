---
name: update-pr
description: Commit relevant local changes and push them to the current GocciaScript pull request branch. Use when the user enters /update-pr.
disable-model-invocation: true
---

# Update PR

This command is explicit permission to commit relevant changes and push them to the current PR branch.

## Rules

- **Never amend commits.** Always create new commits.
- **Never force push.** Use `git push` without `--force` or `--force-with-lease`.

## Steps

1. Inspect the repository state:
   - `git status --short --branch`
   - `git diff`
   - `git diff --staged`
   - `git log --oneline -5`
2. Confirm the current branch is not `main`. If it is `main`, stop and ask for the intended PR branch.
3. Confirm the branch has an associated PR with `gh pr view`. If there is no PR, ask whether to run `/create-pr` instead.
4. **Merge the `main` baseline** if the branch is behind `origin/main`:

```bash
git fetch origin main
git merge origin/main --no-edit
```

   If the merge has conflicts, resolve them and commit the merge before continuing.
5. Confirm there are changes to commit. If there are none (beyond the merge), skip to step 7.
6. Stage only relevant files. Exclude secrets and unrelated user changes.
7. Commit with a concise message passed via HEREDOC.
8. Push the branch, setting upstream tracking if needed:

```bash
git push -u origin HEAD
```

9. Review the current PR title and body against the latest implementation:
   - Read `.github/pull_request_template.md`, or the relevant template under `.github/PULL_REQUEST_TEMPLATE/` if the repository uses multiple templates.
   - Fetch the PR body with `gh pr view --json body,url,title`.
   - Compare the title, Summary, Testing, linked issues, and any scope notes with the actual commits and verification just performed.
   - If the title is stale, too narrow, or no longer matches the implementation, update it with `gh pr edit --title "$PR_TITLE"`.
   - If the body is stale, incomplete, or no longer matches the implementation, update it with `gh pr edit --body-file <file>`.
   - Preserve the repository pull request template structure and avoid removing useful reviewer context.
10. Report the commit hash, pushed branch, PR URL, whether the PR title or body was updated, and verification performed.

Do not skip git hooks or verification unless the user explicitly asks.
