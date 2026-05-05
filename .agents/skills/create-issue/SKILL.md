---
name: create-issue
description: Create a well-structured GocciaScript GitHub issue from a tagline or short description, using the repository issue template and project issue style. Use when the user enters /create-issue.
disable-model-invocation: true
---

# Create Issue

Create a GitHub issue from the user's tagline or short description.

## Steps

1. Parse the user's tagline or short description from the command arguments. If it is missing, ask for it.
2. Read `.github/ISSUE_TEMPLATE/default.md`.
3. Use the established GocciaScript issue style. Good issues in this repo typically include:
   - A specific, plain-language title without an area prefix.
   - A short problem summary.
   - Reproduction commands or a minimal code sample for bugs.
   - Current behavior and expected behavior.
   - Spec context or project context when relevant.
   - Test impact, blocked tests, or user impact.
   - Likely fix area, scope notes, constraints, and related issues.
4. Investigate the codebase enough to avoid creating a vague or duplicate issue. Search for related code, docs, tests, and existing issues. If the tagline cannot be turned into a concrete issue without guessing, ask clarifying questions.
5. Draft the issue using the repository template. Preserve the template's headings unless another project issue pattern is clearly more appropriate for the issue type.
6. Choose labels by matching existing project label conventions. Use labels, not title prefixes, to capture area and type. Do not invent new labels unless the user asks.
7. Show the title, labels, and body draft to the user before creating the issue unless the user explicitly asked to create it without review.
8. Create the issue with GraphQL first. Include `labelIds` only when labels were selected:

```bash
gh api graphql \
  -f query='mutation($repositoryId:ID!, $title:String!, $body:String!, $labelIds:[ID!]) {
    createIssue(input: {
      repositoryId: $repositoryId,
      title: $title,
      body: $body,
      labelIds: $labelIds
    }) {
      issue { url number }
    }
  }' \
  -F repositoryId="$REPOSITORY_ID" \
  -f title="$ISSUE_TITLE" \
  -f body="$ISSUE_BODY"
```

When labels were selected, append one `-F labelIds[]="$LABEL_ID"` argument per label ID.

9. If GraphQL is rate-limited or unavailable, create the issue with REST:

```bash
gh api "repos/$OWNER/$REPO/issues" \
  -f title="$ISSUE_TITLE" \
  -f body="$ISSUE_BODY" \
  -f labels[]="$LABEL_NAME" \
  --jq '.html_url'
```

10. Return the issue URL to the user.
