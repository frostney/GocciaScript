---
name: review-pr
description: >-
  Resolves outstanding review comments on the current pull request by replying
  in-thread and pushing fixes in any GitHub repository, without leaving new
  top-level PR or issue comments. Prefers the /resolve-reviews skill when it is
  available and falls back to a standalone workflow otherwise. Use when the
  user runs /review-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access.
---

# Review PR

## Instructions

Resolve review comments on the current pull request.

### Prefer `/resolve-reviews` when available

If a `resolve-reviews` skill (or `/resolve-reviews` command) is available in this environment, **prefer it** for the per-thread resolution workflow.

- Delegate per-thread resolution mechanics (listing threads, navigating to comment locations, replying inline, marking threads resolved) to `/resolve-reviews`.
- **The Rules section in this skill always applies, even when `/resolve-reviews` is driving.** Rules are non-negotiable overrides — if `/resolve-reviews`'s default behavior would conflict with a rule (e.g. posting a top-level summary comment, force-pushing, reverting unrelated changes), follow the rule, not `/resolve-reviews`.
- The **Steps** below are the standalone fallback. Use them only when `/resolve-reviews` is not available.

To check availability, look for a skill or command named `resolve-reviews` (e.g. `~/.cursor/skills/resolve-reviews/`, `.cursor/skills/resolve-reviews/`, or `.agents/skills/resolve-reviews/`). If none is registered, run the standalone workflow below.

### Rules

- No new top-level PR comments, PR review summaries, or issue comments. Replies to existing review threads are allowed when they help resolve a thread.
- Keep the final summary in chat. Only post it to GitHub if the user explicitly asks.
- Preserve unrelated work in the tree. Never revert changes you did not author.
- Run relevant verification before committing fixes.
- Avoid commands that create top-level comments: `gh pr comment`, REST issue-comment endpoints, or any review body not tied to an existing thread.

### Steps

1. Confirm the current branch has an open PR:

   ```bash
   gh pr view --json url,number,title,reviewDecision
   ```

2. Merge the remote default baseline if the branch is behind:

   ```bash
   BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')
   git fetch origin "$BASE_BRANCH"
   git merge "origin/$BASE_BRANCH" --no-edit
   ```

   Resolve conflicts and commit the merge before addressing reviews.

3. List unresolved review threads and inline comments. Address each one in code where it requires a code change.
4. When a thread needs acknowledgement, clarification, or a follow-up question, reply **inline on the originating thread** — never via a new top-level comment.
5. Run relevant verification: typecheck, lint, tests, and targeted UI checks (visual, accessibility, responsive, theme) for user-facing changes.
6. Invoke the `/update-pr` skill to commit and push the review fixes. Do not run commit/push commands directly — `/update-pr` enforces the no-amend, no-force-push, baseline-merge, and PR title/body reconciliation rules. If the `/update-pr` skill is not invokable in this environment (not registered, not available as a command), follow the rules and steps documented in the `update-pr` skill manually to commit and push.
7. Report in chat: the threads addressed, the commits pushed, verification run, and the PR URL.
