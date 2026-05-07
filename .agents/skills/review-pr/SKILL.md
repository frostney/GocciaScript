---
name: review-pr
description: Resolve review comments on the current GocciaScript pull request without posting top-level PR comments. Use when the user enters /review-pr.
---

# Review PR

Resolve review comments on the current PR by following `.agents/skills/resolve-reviews/SKILL.md`, with these project-specific overrides.

## Rules

- Do not leave top-level PR comments, PR review summaries, or issue comments.
- Replies to existing review comments or review threads are allowed when needed to resolve those comments.
- Keep the final summary in chat unless the user explicitly asks to post it to GitHub.
- Preserve unrelated work in the working tree. Do not revert changes you did not make.
- Run relevant verification before committing review fixes.
- **Merge the `main` baseline** before starting review work if the branch is behind `origin/main`:

```bash
git fetch origin main
git merge origin/main --no-edit
```

   If the merge has conflicts, resolve them and commit the merge before addressing review comments.

Avoid commands that create top-level comments, including `gh pr comment`, REST issue comment endpoints, or a review body that is not tied to an existing thread.
