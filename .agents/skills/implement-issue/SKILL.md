---
name: implement-issue
description: Validate a GitHub issue against the GocciaScript codebase, present implementation options, implement the chosen fix, and prepare a PR. Use when the user enters /implement-issue with an issue number.
disable-model-invocation: true
---

# Implement Issue

Validate and implement the requested GitHub issue.

## Steps

1. Parse the issue number. If it is missing or not numeric, ask the user for the issue number.
2. Fetch the issue with GraphQL first, including title, body, state, labels, assignees, URL, comments, and whether the issue is actually a pull request.
3. If GraphQL is rate-limited or unavailable, fetch the issue with REST:

```bash
gh api "repos/$OWNER/$REPO/issues/$ISSUE_NUMBER"
```

4. Thoroughly review and validate the issue before coding:
   - The issue exists.
   - The issue is open.
   - The issue is not a pull request.
   - Labels or comments do not indicate `blocked`, `duplicate`, `wontfix`, or equivalent.
   - The reported behavior has been checked against the current codebase, not just accepted from the issue text.
   - The problem statement is verified as an actual issue in this codebase by reading the relevant implementation, running or adding a focused reproduction when practical, and comparing observed behavior with the expected behavior.
   - The expected behavior and acceptance criteria are clear enough to implement.
5. If validation fails or requirements are ambiguous, stop and ask the user.
6. Present the user with different implementation options, including tradeoffs, recommended verification, and the option you recommend. Do not code until the user chooses an option or explicitly asks you to proceed with the recommendation.
7. Check whether a focused branch or worktree for the issue already exists locally or on `origin`. Prefer reusing the existing focused worktree when present. If a branch exists without a worktree, use or create a worktree for it when that best isolates the work. If no focused branch exists, create one named from the issue, for example `issue-123-short-slug`, and use a worktree when practical.
8. Implement the smallest complete change that satisfies the chosen approach.
9. Add or update tests and documentation according to `CONTRIBUTING.md`.
10. Run targeted verification, then broader verification when the change has wider impact.
11. Finish by running the `/create-pr` workflow. Include `Closes #<number>` in the PR body.
