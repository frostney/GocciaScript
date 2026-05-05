---
name: project-workflows
description: Project-specific workflow commands for GocciaScript. Use when the user enters /review-pr, /create-pr, or /implement-issue to resolve PR reviews, create pull requests, or validate and implement GitHub issues.
---

# Project Workflows

Treat these as repo-local slash-style commands. When a user message starts with one of the command names below, follow that workflow.

## Common Rules

- Follow `AGENTS.md` and `CONTRIBUTING.md` before substantive code edits.
- Preserve unrelated work in the working tree. Do not revert changes you did not make.
- Use `gh` for GitHub API calls.
- Prefer the GitHub GraphQL API. Use the GitHub REST API only when GraphQL is rate-limited, unavailable, or missing a required operation.
- Do not skip git hooks or verification unless the user explicitly asks.
- Do not commit secrets or credential files.

## `/review-pr`

Resolve review comments on the current PR by following `.agents/skills/resolve-reviews/SKILL.md`, with this override:

- Do not leave top-level PR comments, PR review summaries, or issue comments.
- Replies to existing review comments or review threads are allowed when needed to resolve those comments.
- Keep the final summary in the chat unless the user explicitly asks to post it to GitHub.

Avoid commands that create top-level comments, including `gh pr comment`, REST issue comment endpoints, or a review body that is not tied to an existing thread.

## `/create-pr`

This command is explicit permission to commit relevant changes, push the branch, and create a pull request.

1. Inspect the repository state:
   - `git status --short --branch`
   - `git diff`
   - `git diff --staged`
   - `git log --oneline -5`
2. Confirm there are changes to commit. If there are none, stop.
3. If the current branch is `main`, create a focused branch before committing. Use an issue number or change summary for the branch name when available; otherwise ask the user.
4. Stage only relevant files. Exclude secrets and unrelated user changes.
5. Commit with a concise message passed via HEREDOC.
6. Push the branch with upstream tracking when needed: `git push -u origin HEAD`.
7. Build the PR body from the repository's pull request template. Read `.github/pull_request_template.md`, or the relevant template under `.github/PULL_REQUEST_TEMPLATE/` if the repository uses multiple templates. Fill the template faithfully and preserve its structure.
8. Create the pull request with GraphQL first:

```bash
gh api graphql \
  -f query='mutation($repositoryId:ID!, $base:String!, $head:String!, $title:String!, $body:String!) {
    createPullRequest(input: {
      repositoryId: $repositoryId,
      baseRefName: $base,
      headRefName: $head,
      title: $title,
      body: $body
    }) {
      pullRequest { url number }
    }
  }' \
  -F repositoryId="$REPOSITORY_ID" \
  -f base="$BASE_BRANCH" \
  -f head="$HEAD_BRANCH" \
  -f title="$PR_TITLE" \
  -f body="$PR_BODY"
```

9. If GraphQL is rate-limited or unavailable, create the pull request with REST:

```bash
gh api "repos/$OWNER/$REPO/pulls" \
  -f title="$PR_TITLE" \
  -f head="$HEAD_BRANCH" \
  -f base="$BASE_BRANCH" \
  -f body="$PR_BODY" \
  --jq '.html_url'
```

Return the PR URL to the user.

## `/implement-issue <number>`

Validate and implement the requested GitHub issue.

1. Parse the issue number. If it is missing or not numeric, ask the user for the issue number.
2. Fetch the issue with GraphQL first, including title, body, state, labels, assignees, URL, and whether the issue is actually a pull request.
3. If GraphQL is rate-limited or unavailable, fetch the issue with REST:

```bash
gh api "repos/$OWNER/$REPO/issues/$ISSUE_NUMBER"
```

4. Thoroughly review and validate the issue before coding:
   - The issue exists.
   - The issue is open.
   - The issue is not a pull request.
   - The reported behavior has been checked against the current codebase, not just accepted from the issue text.
   - The problem statement is verified as an actual issue in this codebase by reading the relevant implementation, running or adding a focused reproduction when practical, and comparing observed behavior with the expected behavior.
   - The expected behavior and acceptance criteria are clear enough to implement.
   - Labels or comments do not indicate `blocked`, `duplicate`, `wontfix`, or equivalent.
5. If validation fails or requirements are ambiguous, stop and ask the user.
6. Present the user with different implementation options, including tradeoffs, recommended verification, and the option you recommend. Do not code until the user chooses an option or explicitly asks you to proceed with the recommendation.
7. Check whether a focused branch or worktree for the issue already exists locally or on `origin`. Prefer reusing the existing focused worktree when present. If a branch exists without a worktree, use or create a worktree for it when that best isolates the work. If no focused branch exists, create one named from the issue, for example `issue-123-short-slug`, and use a worktree when practical.
8. Implement the smallest complete change that satisfies the chosen approach.
9. Add or update tests and documentation according to `CONTRIBUTING.md`.
10. Run targeted verification, then broader verification when the change has wider impact.
11. Finish by running the `/create-pr` workflow. Include `Closes #<number>` in the PR body.
