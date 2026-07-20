---
name: create-pr
description: >-
  Commits relevant changes, pushes a focused branch, and opens a templated draft
  pull request. Use when the user runs /create-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access.
---

# Create PR

## Instructions

Explicit permission to commit relevant changes, push the branch, and open a draft pull request.

### Steps

1. Inspect the repository:
   - `git status --short --branch`
   - `git diff`
   - `git diff --staged`
   - `git log --oneline -5`
2. Resolve the base branch from the remote default (do not hardcode `main`):

   ```bash
   BASE_BRANCH=$(gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name')
   ```

3. Resolve whether there is work to publish. If there are no relevant working-tree changes and no commits ahead of `origin/$BASE_BRANCH`, stop. If changes are already committed, continue without creating an empty commit.
4. If the current branch is the base branch, create a focused branch first. Use the issue number or a change summary; ask when ambiguous.
5. Stage only relevant files. Exclude secrets and unrelated local changes.
6. Commit with a concise Conventional Commit message passed via HEREDOC:
   - Subject format: `type(scope): summary`.
   - Use one of: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`, `revert`.
   - Pick the narrowest accurate scope; omit the scope only when no meaningful scope exists.
   - Use imperative mood, lowercase the summary after the type/scope, and do not end the subject with a period.
7. Push with upstream tracking when needed: `git push -u origin HEAD`.
8. Resolve the PR template:
   - Search `.github/pull_request_template.md` and `.github/PULL_REQUEST_TEMPLATE/`.
   - Read `.github/pull_request_template.md`, or pick the matching template under `.github/PULL_REQUEST_TEMPLATE/` when multiple templates are discovered.
   - Absence protocol: after the template search finds no PR template, state that no project PR template was found and use a minimal structure: Summary, Testing, Linked issues.
   - Fill the template faithfully and preserve its structure.
9. Open as a **draft** via GraphQL first:

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

10. If GraphQL is rate-limited or unavailable, fall back to REST as a **draft**:

    ```bash
    gh api "repos/$OWNER/$REPO/pulls" \
      -f title="$PR_TITLE" \
      -f head="$HEAD_BRANCH" \
      -f base="$BASE_BRANCH" \
      -f body="$PR_BODY" \
      -F draft=true \
      --jq '.html_url'
    ```

11. Return the PR URL.

Do not skip git hooks or verification unless the user explicitly asks.
