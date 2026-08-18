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

Turn the user's input into an implementation-ready issue grounded in the current
repository, then create it after the required review boundary.

## Gates

- Investigate before drafting: read the applicable project instructions and
  vision, search open and closed issues for duplicates, and inspect the affected
  code, tests, and docs.
- When `grill-with-docs` or `grill-me` is registered, run its actual
  user-question loop before drafting. Prefer `grill-with-docs`. If neither is
  available, note that once and continue.
- Stop before drafting when the request conflicts with project vision, is a
  duplicate, or needs material facts that cannot be established. For a
  duplicate, return the existing issue and stop without asking whether to file
  the same request anyway or mutate the existing issue.
- Before posting, resolve the authenticated GitHub username and exact model name
  from current forge and host evidence. Stop if either is unavailable; never
  guess or substitute a generic label.

## Automatic mode

Automatic mode applies only when the original prompt says `automatic` or
explicitly requests it. Complete every gate, then choose the template, title,
labels, and body from project evidence and create the issue without draft
approval. Material ambiguity or risk disables automatic mode.

## Workflow

1. Discover the repository's issue templates and existing label conventions.
2. Investigate the request using the gates above. Ground any progress claim in
   evidence gathered this run.
3. Run the registered grill skill and incorporate the result.
4. Draft against the matching template. Keep only decision-, implementation-,
   and verification-relevant content:
   - a specific plain-language title;
   - problem, current and expected behavior, scope, constraints, and acceptance
     criteria;
   - reproduction and regression expectations for bugs;
   - user/test impact, likely affected area, and related work where relevant.
5. For UI/UX work, add the affected states, current and expected visual evidence,
   accessibility expectations, responsive/theme scope, and applicable design
   system components or tokens.
6. Choose only existing labels unless the user asks to create one.
7. Show the proposed title, labels, and body unless the user waived review or
   automatic mode applies.
8. End the issue body with this visually separate GitHub Note, replacing both
   values with the exact identities resolved for this run:

   > [!NOTE]
   > Created on behalf of @username using ModelName.

9. Create the issue with the available forge tooling and return its URL.

Lead with the outcome. Omit boilerplate, repeated summaries, and a narration of
the workflow.
