---
name: review-pr
description: >-
  Resolves current pull-request review findings in place, validates and pushes
  fixes, and can autonomously converge and merge an opted-in pull request. Use
  when the user runs /review-pr or /review-pr automatic-merge.
license: Unlicense OR MIT
compatibility: >-
  Requires Python 3.11 or newer, the GitHub CLI (gh) authenticated to the target
  repository, the internal `delivery-wait` skill, and network access.
---

# Review PR

Converge exactly one pull request without creating a second review conversation.
With the exact `automatic-merge` qualifier, merge an ordinary PR only after the
same exact-head convergence contract passes.

## Invariants

- Preserve unrelated work. Never amend, force-push, or revert changes you did
  not author.
- Reply only in the originating review thread. Every inline automation thread
  requires a maintainer-workflow reply stating its evidence-backed disposition
  before readiness or merge, including invalid, obsolete, duplicate, and
  out-of-scope findings. Do not post top-level PR summaries or issue comments.
  In `automatic-merge` mode, a documented automation retrigger command is the
  only allowed top-level comment.
- Before any substantive review post or reply, resolve the authenticated GitHub
  username and exact model name from current forge and host evidence. Stop if
  either is unavailable; never guess. End each reply with this GitHub Note and
  keep the full reply at 300 characters or fewer:

  > [!NOTE]
  > Created on behalf of @username using ModelName.

  Do not append attribution to an exact automation retrigger command because
  extra text can invalidate it.
- Validate findings before changing code and run the relevant project checks
  after fixes.
- Discover active review tools from current repository configuration, branch
  protection, checks, and PR activity. Do not hardcode one provider or require
  an integration that is disabled, historical, or merely installed.
- Treat a terminal automation check as completion evidence only. Success and
  neutral conclusions never establish that the automation reported no findings;
  consume and classify the helper's exact-head `findingSurfaces` before any
  readiness or merge conclusion.
- Treat review, approval, thread-readiness, finding, and CI evidence as valid
  only for the exact current PR head. A new commit or baseline update resets
  every affected gate.
- Own no label routing, milestone scheduling, stack scheduling, cross-PR
  admission, or project-specific CI policy. If the PR is a native stack member,
  converge this layer and return its state to the stack owner without merging.

Read [references/convergence.md](references/convergence.md) before deciding that
a PR is ready, pending, blocked, or merged.

Use `scripts/review_wait.py` for review inspection, deterministic waiting,
inline replies, and thread resolution. Invoke it with `--json`; the harness must
passively await a running command rather than wake a model to report unchanged
state. The repository policy defaults to
`.github/delivery/review-automations.json` and may be overridden explicitly.
Use a caller-owned checkpoint below gitignored `.agent/waits/`.

## Automatic merge

The exact `automatic-merge` qualifier authorizes relevant fixes, validation,
new commits, permitted pushes, documented automation retriggers, monitoring,
one ordinary squash merge, source-branch deletion, and local cleanup under
`git-workflow`. Normal `/review-pr` remains non-merging. An explicit read-only
instruction remains non-mutating and disables automatic merge.

An active review automation is a gate when repository policy or the current PR
shows it was intentionally invoked. Inspect inline threads plus top-level
reviews, summaries, suggestions, and nitpicks. A rate-limited, incomplete,
errored, missing, or head-ambiguous verdict is pending rather than passed.

## Workflow

1. Confirm the repository and exact PR identity. Read its current head, diff,
   required checks, applicable project instructions, active review automation,
   terminal states, unresolved-thread count, and unanswered inline-automation-
   thread count.
2. If an ordinary branch needs a baseline update, use `/update-pr`. A stack owner
   must perform any stack-wide synchronization before asking this skill to
   re-evaluate the affected layer.
3. Run the review helper's `inspect` operation for the exact PR head. It returns
   active automation evidence, one explicit `findingSurfaces` collection across
   inline threads, exact-head reviews, and automation top-level comments,
   replies, and authoritative thread state. Inspect every returned body and
   classify each surface in this workflow. `judgment-required` means automation
   completed but its content still needs that classification; it is never a
   pass. The helper supplies facts and exact mutations, never judgment.
4. Evaluate every current finding. Fix validated in-scope findings; reply inline
   to every automation thread through the helper's idempotent `reply` operation;
   resolve completed threads through its explicit `resolve` operation. Dismiss invalid,
   obsolete, duplicate, or out-of-scope findings only with evidence. Never
   silently ignore a nitpick, and never substitute a top-level comment when an
   inline comment cannot accept a reply. Resolve attribution identities before
   the first reply and append the required Note to every substantive reply.
5. Run checks relevant to the changed behavior, including rendered UI and
   accessibility checks for user-facing changes.
6. Use `/update-pr` to commit and push. If unavailable, follow its documented
   no-amend, no-force-push workflow directly.
7. Re-read the exact head, required checks, terminal automation verdicts,
   actionable findings, unresolved threads, and unanswered inline automation
   threads. Apply the convergence and `retry_at` rules in the reference. A new
   head restarts this step with no inherited gate evidence.
8. In normal mode, return the result contract without merging. In
   `automatic-merge` mode, launch the helper's foreground `wait` operation with
   the exact head, repository policy, checkpoint, and safely derived deadline.
   Resume this workflow only when the command returns a meaningful transition.
   Use a
   documented retrigger only when current evidence permits it; its required
   command may use the narrow top-level exception. Never guess a timer, quota,
   provider policy, or retry count. If the host cannot passively await a
   subprocess, return `pending` with that unsupported capability instead of
   using model heartbeats.
9. Stop without merging for a material product decision, unrelated failure,
   unsafe or divergent PR, unavailable terminal external dependency, or
   unresolved required finding. Report the exact blocker.
10. In `automatic-merge` mode, squash-merge through `git-workflow` only when the
    ordinary PR is `ready` under the exact final-head contract. Sync the local
    default branch, remove only clean worktrees owned by this run, and report
    the merged PR, final head, validation, reviews, and cleanup. For a native
    stack member, return `ready` without merging so the stack owner can recheck
    its selected prefix and invoke the atomic stack merge.
