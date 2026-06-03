---
name: create-issue
description: >-
  Creates a well-structured GitHub issue from a tagline or short description in
  any repository, investigating for duplicates and invoking the grill skill
  (grill-with-docs / grill-me) as a mandatory gate before drafting, using the
  project's issue template and label conventions, and capturing UI/UX context
  (screenshots, design references, accessibility, responsive, theme) when the
  change is user-facing. Use when the user runs /create-issue or asks to file a
  GitHub issue.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access.
---

# Create issue

## Instructions

Create a GitHub issue in the current repository from the user's tagline or short description.

### Non-negotiable gates (do not skip, do not rationalize)

These gates are mandatory. Do not skip them because the tagline "looks clear" or because creating an issue feels like a quick task.

1. **GATE A — Grill before drafting.** When a grill skill is registered, you **must** invoke it (step 4) before drafting the issue body. Do not draft without it.
2. **GATE B — Investigate before drafting.** You **must** complete the investigation in step 3 (search for duplicates, read the implementation area) before drafting. Do not draft from the tagline alone.

**Forbidden rationalizations** — if you catch yourself writing any of these, stop and follow the gate instead:

- ❌ "The tagline is clear enough, I'll just draft it." → No. Grill and investigate first; a sharper issue comes from the questions you haven't asked yet.
- ❌ "Grill isn't necessary for a simple issue." → No. If it's registered, run it.
- ❌ "I treated `grill-with-docs` as 'answer with doc-grounding' instead of actually running the grilling loop." → No. Invoking grill means **executing the grill skill itself** — its real, interactive question loop — not answering in a doc-grounded style, not paraphrasing what it would ask. Load the skill and run it.
- ❌ "I'll ask a couple of clarifying questions of my own; that's basically grilling." → No. That is not the grill skill. Run the actual `grill-with-docs` / `grill-me` skill.

### Use the grill skill for thoroughness (always when available)

Before drafting, the agent **always invokes the grill skill** when it is registered in this environment — not only when something is ambiguous. This is GATE A above. The grill output is folded into the issue body so the result is more thorough than the raw tagline would produce.

**Invoking the grill skill means literally running that skill — not imitating its spirit.** `grill-with-docs` and `grill-me` are separate skills with their own multi-question interrogation loop. To invoke one you **read its `SKILL.md` and execute its procedure**: actually ask the user the grilling questions it generates and wait for the answers, iterating until the loop completes. The following are **NOT** invoking it and are forbidden substitutes:

- Treating the mention of `grill-with-docs` as a style instruction — "answer with doc-grounding," "be thorough," "cite the docs" — and then proceeding. ❌
- Summarizing or paraphrasing the questions grilling *would* ask instead of asking them. ❌
- Asking one or two clarifying questions of your own and calling that grilling. ❌
- Skipping it because you believe you already understand the tagline. ❌

If you cannot run the grill skill, do not silently downgrade it to "doc-grounded answering" — say explicitly that no grill skill was found (see discovery hint) and proceed on the input as given.

- **`/grill-with-docs` is preferred.** Use `/grill-me` only when `/grill-with-docs` is not registered.
- Discovery hint: look for a skill or command named `grill-with-docs` or `grill-me` (e.g. `~/.cursor/skills/grill-with-docs/`, `~/.cursor/skills/grill-me/`, `.cursor/skills/...`, `.agents/skills/...`).
- If neither is registered, state explicitly that no grill skill was found, then proceed with the workflow on the input as given.

### Steps

1. Parse the tagline or short description. If missing, ask.
2. Resolve the issue template:
   - Prefer `.github/ISSUE_TEMPLATE/` (multiple templates); pick the one matching the issue type (bug, feature, chore, etc.).
   - Fall back to `.github/ISSUE_TEMPLATE/default.md` or `.github/ISSUE_TEMPLATE.md`.
   - If no template exists, use a minimal structure: Summary, Reproduction (bugs), Current vs Expected, Scope, Related.
3. **Investigate before drafting (GATE B):**
   - Search code, docs, tests, and existing open/closed issues for duplicates and related work.
   - Read the implementation area the issue touches. Do not draft from the tagline alone.
   - If the tagline cannot become a concrete issue without guessing, stop and ask.
4. **Run the grill skill (GATE A).** When `grill-with-docs` / `grill-me` is registered, **read that skill and execute its actual question loop now** on the tagline plus your investigation findings — ask the questions, wait for answers, iterate to completion — then fold its output into the issue body. Do not substitute a "doc-grounded" answer or your own ad-hoc questions for the skill. If none is registered, say so explicitly and continue.
5. Draft the issue. A good issue typically includes:
   - A specific, plain-language title with no area prefix (use labels for area/type).
   - A short problem summary.
   - For bugs: reproduction command or minimal code/UI sample; current vs expected behavior.
   - Project context (spec, RFC, related issue) when relevant.
   - Test impact, user impact, or blocked work.
   - Likely fix area, scope notes, constraints, and related issues.
6. **If the change is UI/UX, also include:**
   - Affected screens, routes, or components.
   - Current visual state: screenshot, short recording, or precise description (layout, copy, state).
   - Expected visual state: screenshot, mock, design link (Figma, etc.), or precise description.
   - Accessibility expectations: keyboard navigation, focus order, visible focus, ARIA roles/labels, color contrast (target WCAG AA or the project's standard), motion / `prefers-reduced-motion`.
   - Responsive scope: which breakpoints and devices apply, and which themes (light/dark/system).
   - Design system or component library in use, and the specific tokens or components involved.
7. Choose labels by matching existing repo conventions. Use labels (not title prefixes) for area and type. Do not invent labels unless the user asks.
8. Show the title, labels, and body to the user before creating, unless the user asked to create without review.
9. Resolve the repository ID, then create the issue with GraphQL:

   ```bash
   REPOSITORY_ID=$(gh api graphql \
     -f query='query($owner:String!,$name:String!){repository(owner:$owner,name:$name){id}}' \
     -f owner="$OWNER" -f name="$REPO" --jq '.data.repository.id')

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

10. If GraphQL is rate-limited or unavailable, fall back to REST:

    ```bash
    gh api "repos/$OWNER/$REPO/issues" \
      -f title="$ISSUE_TITLE" \
      -f body="$ISSUE_BODY" \
      --jq '.html_url'
    ```

    When labels were selected, append one `-f labels[]="$LABEL_NAME"` argument per label name.

11. Return the issue URL.
