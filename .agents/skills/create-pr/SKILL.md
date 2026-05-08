---
name: create-pr
description: Commit relevant changes, push the branch, and create a GocciaScript pull request using the repository PR template. Use when the user enters /create-pr.
---

# Create PR

This command is explicit permission to commit relevant changes, push the branch, and create a pull request.

## Steps

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
8. Create the pull request as a **draft** with GraphQL first:

```bash
gh api graphql \
  -f query='mutation($repositoryId:ID!, $base:String!, $head:String!, $title:String!, $body:String!) {
    createPullRequest(input: {
      repositoryId: $repositoryId,
      baseRefName: $base,
      headRefName: $head,
      title: $title,
      body: $body,
      draft: true
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

9. If GraphQL is rate-limited or unavailable, create the pull request as a **draft** with REST:

```bash
gh api "repos/$OWNER/$REPO/pulls" \
  -f title="$PR_TITLE" \
  -f head="$HEAD_BRANCH" \
  -f base="$BASE_BRANCH" \
  -f body="$PR_BODY" \
  -F draft=true \
  --jq '.html_url'
```

10. Return the PR URL to the user.

Do not skip git hooks or verification unless the user explicitly asks.
