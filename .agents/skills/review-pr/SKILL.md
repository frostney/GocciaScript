---
name: review-pr
description: >-
  Resolves current pull-request review findings in place, validates and pushes
  fixes, and can autonomously converge and merge an opted-in pull request. Use
  when the user runs /review-pr or /review-pr automatic-merge.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access.
---

# Review PR

## Instructions

Resolve the current PR's actionable review findings without creating a second
review conversation. With the exact `automatic-merge` qualifier, converge the
PR and merge it.

### Invariants

- Preserve unrelated work. Never amend, force-push, or revert changes you did
  not author.
- Reply only in the originating review thread when a reply is useful. Do not
  post top-level PR summaries or issue comments. In `automatic-merge` mode, a
  documented review-tool retrigger command is the only allowed top-level
  comment.
- Validate findings before changing code and run the relevant project checks
  after fixes.
- Discover active review tools from current repository configuration, branch
  protection, checks, and PR activity. Do not hardcode one provider or require
  an integration that is disabled, historical, or merely installed.
- Treat review, approval, and CI evidence as valid only for the current PR head.
  A new commit or baseline merge resets every affected gate.

### Automatic merge

The exact `automatic-merge` qualifier authorizes relevant fixes, validation,
new commits, plain pushes, documented reviewer retriggers, monitoring, one
squash merge, source-branch deletion, and local cleanup under `git-workflow`.
Normal `/review-pr` remains non-merging. An explicit read-only instruction
remains non-mutating and disables automatic merge.

An active review tool is a merge gate when repository policy or the current PR
shows it was intentionally invoked. Inspect inline threads plus top-level
reviews, summaries, suggestions, and nitpicks. For CodeRabbit when active, a
rate limit, quota response, incomplete run, or missing verdict is pending rather
than passed; apply equivalent evidence-based semantics to other tools.

### Workflow

1. Confirm the branch has an open PR and merge the remote default branch if
   behind.
2. Read the current head, PR diff, unresolved threads, top-level review
   findings, active review tools and their terminal states, required checks,
   affected code, and applicable project instructions.
3. If `resolve-reviews` is registered, use it for thread mechanics while keeping
   this skill's invariants. Otherwise handle the threads directly.
4. Evaluate every current finding. Fix validated in-scope findings; reply inline
   when acknowledgement, clarification, or a question is needed; resolve
   completed threads. Dismiss invalid, obsolete, duplicate, or out-of-scope
   findings only with evidence. Never silently ignore a nitpick.
5. Run checks relevant to the changed behavior, including rendered UI and
   accessibility checks for user-facing changes.
6. Use `/update-pr` to commit and push. If unavailable, follow its documented
   no-amend, no-force-push workflow directly.
7. In normal mode, report the outcome, findings addressed, commits, observed
   validation, review-tool state, and PR URL.
8. In `automatic-merge` mode, repeat the workflow against the new head. Wait
   with bounded backoff for required checks and every active review tool. When a
   tool is incomplete, errored, or rate-limited, use its documented retrigger
   mechanism when current evidence says it is allowed; a required command
   comment may use the narrow top-level exception. Do not impose a retry-count
   limit beyond the host platform.
9. Stop without merging for a material product decision, unrelated failure,
   unsafe or divergent PR, unavailable terminal external dependency, or
   unresolved required finding. Report the exact blocker.
10. Once the current head is ready, every required check is green, every active
    review tool has a completed verdict, and no actionable finding remains,
    squash-merge and delete the source branch through `git-workflow`. Sync the
    local default branch, remove only clean worktrees owned by this run, and
    report the merged PR, final head, validation, reviews, and cleanup.
