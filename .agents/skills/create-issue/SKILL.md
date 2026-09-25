---
name: create-issue
description: >-
  Investigates and creates a project-aligned GitHub issue from a tagline or short
  description, using the repository's template, evidence, and labels. Use when
  the user runs /create-issue or asks to file a GitHub issue.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) 2.99 or newer authenticated to the target
  repository with push access, and network access.
---

# Create issue

Turn the user's input into an implementation-ready issue grounded in the current
repository, then create it after the required review boundary.

User instructions override skill defaults. Reuse authorization and settled
decisions within their scope across turns. Before a required pause, complete
independent authorized work, then identify the exact skill file and quote the
rule requiring a new decision or authority.

## Gates

- Investigate before drafting: read the applicable project instructions and
  vision, search open and closed issues for duplicates of the outcome as well
  as the mechanism, and inspect the affected code, tests, and docs.
- For material unresolved requirements or a requested interview, use the actual
  registered `grill-with-docs` or `grill-me` loop; prefer `grill-with-docs`.
  Reuse settled decisions. If neither is available, investigate directly and
  ask only for missing facts that affect the issue.
- Stop before drafting when the request conflicts with project vision, is a
  duplicate, or needs material facts that cannot be established. For a
  duplicate, return the existing issue and stop without asking whether to file
  the same request anyway or mutate the existing issue.
- Before posting, resolve the authenticated GitHub username and exact model name
  from the current GitHub account and agent environment. Stop if either is
  unavailable; never guess or substitute a generic label.

## Automatic mode

Automatic mode applies when the user explicitly requests it, including in a
later turn. Preserve an existing draft-review waiver within its authorized scope. Complete every gate, then choose the template, title,
labels, and body from project evidence and create the issue without draft
approval. Material ambiguity or risk disables automatic mode.

## Workflow

1. Discover the repository's issue templates and existing label conventions.
2. Investigate the request using the gates above. Ground any progress claim in
   evidence gathered this run.
3. Resolve any remaining material question under the gates above.
4. Draft against the matching template. Keep only decision-, implementation-,
   and verification-relevant content:
   - a specific plain-language title;
   - problem, current and expected behavior, scope, constraints, and required
     behavior;
   - reproduction and regression expectations for bugs;
   - user/test impact, likely affected area, and related work where relevant.
5. For UI/UX work, add the affected states, current and expected visual evidence,
   accessibility expectations, responsive/theme scope, and applicable design
   system components or tokens. Upload local screenshots and videos with
   `gh issue create --attach '<file>#<alt text>'` (or `gh issue edit` and
   `gh issue comment` with the same flag); never commit evidence media to the
   repository. Put non-media evidence such as probe output inside a collapsed
   `<details>` block.
6. Choose only existing labels unless the user asks to create one.
7. Show the proposed title, labels, and body unless the user waived review or
   automatic mode applies.
8. End the issue body with this visually separate GitHub Note, replacing both
   values with the exact identities resolved for this run:

   > [!NOTE]
   > Created on behalf of @username using ModelName.

9. Create the issue with the available GitHub tooling, verify the returned
   issue's repository and intended content, and return its URL. If the write
   response is lost or interrupted, search for the issue before retrying.
   Report an unresolved write as uncertain; a request being accepted or
   delegated is not proof that the issue was created.

Lead with the outcome. Omit boilerplate, repeated summaries, and a narration of
the workflow.
