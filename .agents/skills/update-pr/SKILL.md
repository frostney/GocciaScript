---
name: update-pr
description: >-
  Commits relevant changes, merges the remote default when needed, pushes the
  current pull-request branch, and refreshes stale PR metadata. Use when the
  user runs /update-pr or asks to update a pull request.
license: Unlicense OR MIT
compatibility: >-
  Requires git and the GitHub CLI (gh) authenticated to the target repository,
  plus network access.
---

# Update PR

Update the established PR through integration, relevant commits, a normal push
and current metadata. The request includes resolving routine conflicts and
running the declared PR gate. Reuse the selected target and prior authorization;
ask only when the intended PR or a material resolution choice remains unclear.

When the current PR belongs to a native GitHub stack, read
[../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).

1. Apply `git-workflow`. Inspect the current PR, branch, relevant local changes,
   recent commits and fetched remote default. Resolve a behind-base or
   conflicting branch before deciding whether missing CI needs any action.
2. Stop if on the base branch or no open PR exists; report the required next
   workflow.
3. When an ordinary branch is behind the remote base, merge it into the branch.
   Preserve both sides' required behavior in additive conflicts; regenerate
   generated files using the project's tool. Continue through validation when
   the resolution is established. Ask for a material unresolved choice.
   For a verified native stack, capture remote heads and use the guarded
   `gh stack sync` or narrower official stack operation; never use raw rebase or
   force-push commands.
4. Apply `/code-review fix-all` and `/test-against-spec fix` to the changed
   behavior, including any baseline integration. Reuse matching current evidence;
   run missing or invalidated checks. Repair every verified in-scope requirement
   gap through `/implement`'s development loop, then establish the declared PR
   gate. Preserve content, command, environment and coverage bindings. A preview
   needed to test an unpublished fix permits its draft update; resume testing
   on that exact revision before claiming readiness.
5. Stage only relevant files and commit them with a concise Conventional Commit
   subject. Never amend and never skip hooks.
6. Push an ordinary branch normally, setting upstream when needed. Push a
   verified stack only through the guarded official stack workflow.
7. Reconcile the PR title and body with the complete current diff, scope, linked
   issues, and observed verification. Keep the title a Conventional Commit
   subject for the whole change; the squash merge makes it the commit subject on
   the base branch, so widened scope may also change its type. Follow
   [../agent-writing/references/pr-descriptions.md](../agent-writing/references/pr-descriptions.md),
   preserving explicit project-template requirements and replacing obsolete
   summaries or intermediate development history. Reuse media that still shows
   the current behavior; refresh affected walkthrough segments using
   [../create-pr/references/walkthroughs.md](../create-pr/references/walkthroughs.md).
8. Report the updated PR, commit, metadata changes and observed validation.
   Include stack position and rewritten branches when applicable. Distinguish
   passed local checks from pending current-head CI. Return the exact new head
   and next transition to the active publication or delivery caller; that caller
   continues through CI and feedback. Updating a PR does not
   authorize merging it. Report missing walkthrough requirements with remedies;
   media tooling gaps alone do not block the update.
