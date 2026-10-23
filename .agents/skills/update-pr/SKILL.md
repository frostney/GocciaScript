---
name: update-pr
description: >-
  Commits relevant changes, merges the remote base when needed, pushes the
  current pull-request branch, and refreshes stale PR metadata. Use when asked
  to update or push changes to an existing pull request, or when the user runs
  /update-pr.
license: Unlicense OR MIT
compatibility: >-
  Requires git, Python 3.11 or newer, the GitHub CLI (gh) authenticated to
  the target repository, the internal `template-check` skill, and network
  access.
metadata:
  agents-role: entry-point
  agents-text: Push new changes to an existing pull request and refresh its metadata.
---

# Update PR

Update the established PR through integration, relevant commits, a normal push
and current metadata. The request includes resolving routine conflicts and
running the declared PR gate; merging belongs to `/deliver` under the
[delivery settings](../deliver/SKILL.md#delivery-settings). Reuse the selected target
and prior authorization. Ask only when the intended PR, a
[material](../software-engineering-excellence/SKILL.md#blockers-and-decisions)
conflict-resolution choice or another material choice within this update
remains unclear.

When the current PR belongs to a native GitHub stack, read
[../git-workflow/references/github-stacks.md](../git-workflow/references/github-stacks.md).
When PR media no longer shows the current behavior, or the change needs new
media, refresh the affected parts using
[../create-pr/references/walkthroughs.md](../create-pr/references/walkthroughs.md);
reuse media that still shows the current behavior. Media tooling gaps alone do
not block the update.

1. Apply `git-workflow`. Inspect the current PR and its base branch, the
   branch, relevant local changes, recent commits and the fetched remote base
   as `git-workflow` defines it. Resolve a branch that needs the base under
   step 3 before deciding whether missing CI needs any action.
2. Stop if on the base branch or no open PR exists; report the required next
   workflow. Stop likewise when `git-workflow` gives the PR no remote base.
3. Merge the remote base into an ordinary branch only when `git-workflow`'s
   [up-to-date rule](../git-workflow/SKILL.md#update-commit-and-push) calls
   for it. A branch that is behind keeps its head and its checks, including
   when this update commits new changes. Preserve
   both sides' required behavior in additive conflicts; regenerate generated
   files using the project's tool. Continue through validation when
   the resolution is established. For a verified native stack, capture remote
   heads and use the guarded `gh stack sync` or narrower official stack
   operation; never use raw rebase or force-push commands.
4. Apply `/code-review fix-all` and `/test-against-spec fix` to the changed
   behavior, including any baseline integration. Reuse only evidence whose
   content, command, environment and coverage still match; a baseline merge or
   edit invalidates the checks it affects. Run missing or invalidated checks. Repair every verified in-scope requirement
   gap through `/implement`'s development loop, then establish the declared PR
   gate. A preview needed to test an unpublished fix permits its draft update;
   resume testing on that exact revision before claiming readiness.
5. When relevant changes exist, commit them under `git-workflow`: stage only
   those files, use a concise Conventional Commit subject, and never amend or
   skip hooks. Run the
   [publication guard](../git-workflow/SKILL.md#publication-guard) at each
   commit, push and PR write.
6. When the branch has commits its remote lacks, push an ordinary branch
   normally, setting upstream when needed. Push a verified stack only through
   the guarded official stack workflow.
7. Reconcile the PR title and body with the complete current diff, scope and
   linked issues. A push always changes the head, so after a push reconcile
   the body again even when the summary still holds. Keep the title a Conventional Commit
   subject for the whole change; it becomes the squash-merge commit subject, so
   widened scope may also change its type. Follow
   [../agent-writing/references/pr-descriptions.md](../agent-writing/references/pr-descriptions.md),
   preserving explicit project-template requirements and replacing obsolete
   summaries, intermediate development history and author verification
   sections. Keep the
   [review note](../agent-writing/references/pr-descriptions.md#review-and-status-results)
   at the end of the body, updated as it describes. Keep every PR template
   heading verbatim and in order, and under a heading with nothing to say
   write a one-line `None` or the reason. Run
   [`template-check`](../template-check/SKILL.md#running-a-check) on the draft
   with `--kind pr --body FILE` before writing it and on the live body with
   `--pr N` after, and fix every heading problem it reports.
8. Report the updated PR, any new commit, metadata changes, what the
   publication guard rewrote and observed validation. Include stack position
   and rewritten branches when applicable.
   Distinguish passed local checks from pending current-head CI. A draft PR
   stays draft until `delivery-wait` observes every required check green on the
   pushed head; only then may it be marked ready for review. Return the
   exact current head and next transition to the active publication or delivery
   caller; that caller continues through CI and feedback. Started directly,
   continue under the [delivery settings](../deliver/SKILL.md#delivery-settings);
   with `entry-points: deliver`, the pushed head goes on to `/deliver`, which
   awaits its CI and review and takes the PR to the configured endpoint
   (`ready-to-merge` when there is no `ORCHESTRATION.md`). With `stop`, end
   with this report.
   Report missing walkthrough requirements with remedies.
