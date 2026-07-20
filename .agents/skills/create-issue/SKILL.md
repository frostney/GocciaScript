---
name: create-issue
description: >-
  Investigates and creates a project-aligned GitHub issue from a tagline or short
  description, using the repository's template, evidence, and labels. Use when
  the user runs /create-issue or asks to file a GitHub issue.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access.
---

# Create issue

## Instructions

Create a GitHub issue in the current repository from the user's tagline or short description.

### Non-negotiable gates

1. **Investigate before drafting.** Search for duplicates and related work, read
   the affected implementation area, and check project vision and instructions.
2. **Run the grill skill when registered.** Use `grill-with-docs`, falling back
   to `grill-me`, and complete its actual user-question loop before drafting.
   Ad-hoc questions or a doc-grounded summary are not substitutes. If neither
   skill is registered, note that once and continue.

### Automatic mode

Automatic mode is opt-in. It is active only when the user's original `/create-issue` prompt includes the standalone word `automatic` or explicitly asks for automatic mode.

In automatic mode, do **not** skip template discovery, duplicate investigation, `VISION.md` review, grilling, the drafting checkpoint, or issue drafting. Auto-select the issue template, labels, title, and final issue body from project context, then create the issue without pausing for user review.

If the issue would be contrary to `VISION.md`, appears duplicate, needs missing facts that cannot be inferred from the project, or has materially risky scope, automatic mode does not apply: stop and ask the user for clarification.

### Drafting checkpoint

After investigation and grilling, give one concise phase update naming the
material project constraints or findings that shape the draft. Do not narrate an
internal compliance checklist or list context that did not affect the result.

### Steps

1. Parse the tagline or short description. If missing, ask.
2. Resolve the issue template:
   - Search `.github/ISSUE_TEMPLATE/`, `.github/ISSUE_TEMPLATE/default.md`, and `.github/ISSUE_TEMPLATE.md`.
   - Prefer `.github/ISSUE_TEMPLATE/` when multiple templates are discovered; pick the one matching the issue type (bug, feature, chore, etc.).
   - Fall back to `.github/ISSUE_TEMPLATE/default.md` or `.github/ISSUE_TEMPLATE.md` when discovered.
   - Absence protocol: after the template search finds no issue template, state that no project issue template was found and use a minimal structure: Summary, Reproduction (bugs), Current vs Expected, Scope, Related.
3. **Investigate before drafting:**
   - Search for `VISION.md` at the repository root and in relevant product/docs areas. Read every discovered vision document and use it to shape the issue scope, non-goals, and acceptance criteria. If the tagline asks for behavior contrary to the stated product or technical vision, call out the conflict explicitly and ask the user whether to revise the issue, override the vision for this work, or abandon the issue before drafting or creating it.
   - Search code, docs, tests, and existing open/closed issues for duplicates and related work.
   - Read the implementation area the issue touches. Do not draft from the tagline alone.
   - If the tagline cannot become a concrete issue without guessing, stop and ask.
4. **Run the grill gate.** Give the grill skill the tagline plus the material project and investigation context, complete its loop, and fold the result into the issue. Do not implement product code during grilling unless that skill explicitly requires a context update.
5. **Give the drafting checkpoint, then draft the issue.** A good issue typically includes:
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
8. Show the title, labels, and body to the user before creating, unless the user asked to create without review or automatic mode applies. In automatic mode, state the auto-selected template, labels, title, and body rationale, then continue to issue creation without waiting.
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
