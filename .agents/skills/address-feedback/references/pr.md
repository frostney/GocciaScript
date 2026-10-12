# Address PR feedback

Work through exactly one pull request without creating a second review
conversation. With the exact `automatic-merge` qualifier, merge an ordinary PR
only after the same exact-head readiness contract passes.

Apply the parent SKILL.md shared authority, attribution, and evidence rules.

## PR-specific boundaries

- Reply only in the originating review thread. Every inline automation thread
  requires a maintainer-workflow reply stating its evidence-backed disposition
  before readiness or merge, including invalid, obsolete, duplicate, and
  out-of-scope findings. Do not post top-level PR summaries or issue comments.
  In normal and `automatic-merge` modes, the only allowed top-level comment is
  an intentionally active automation's documented retrigger command, posted
  only when all of these hold:
  - the automation has no completed verdict for the exact current head because
    its attempt was rate-limited, skipped, or missing;
  - any availability the provider stated has passed under the `retry_at` rules
    in [pr-readiness.md](pr-readiness.md); a statement without a derivable
    `retry_at` keeps the automation `pending`;
  - no retrigger for that automation was posted on the current head; and
  - the command does not request a paid or usage-based review.

  CodeRabbit is the exception: its adapter alone posts its commands, as
  [CodeRabbit](pr-readiness.md#coderabbit) states.
- Mechanical applicability alone does not validate a finding: a symbol existing,
  a patch applying, or compilation succeeding does not establish its factual
  claim or authority. Classify both axes in step 4.
- Reply-only and metadata-only updates need no implementation review loop.
  Read-only mode never invokes mutating review.
- Own no label, milestone, or stack scheduling, cross-PR admission, or custom
  CI policy. Return a single stack member's readiness to its stack owner.

Read [pr-readiness.md](pr-readiness.md) before deciding that
a PR is ready, pending, blocked, or merged.

Use `scripts/review_wait.py` for review inspection, deterministic waiting,
inline replies, and thread resolution. Invoke it with `--json`; the harness must
passively await a running command rather than wake a model to report unchanged
state. The repository policy defaults to
`.github/delivery/review-automations.json` and may be overridden explicitly.
Use a caller-owned `--state` path below `.agent/waits/`, which the repository
must gitignore.

## Active review automation

An active review automation is a gate when repository policy or the current PR
shows it was intentionally invoked. Inspect inline threads plus top-level
reviews, summaries, suggestions, and nitpicks. A rate-limited, incomplete,
errored, missing, or head-ambiguous verdict is pending rather than passed.

## Workflow

1. Confirm the repository and exact PR identity. Read its current head, diff,
   required checks, applicable project instructions, active review automation,
   terminal states, unresolved-thread count, and unanswered inline-automation-
   thread count. Establish the complete PR specification from the user request,
   linked issue or confirmed mini-spec, required behavior, PR body, product
   docs, ADRs or durable decisions, and the nearest Definitions of Ready and
   Done. Label any claim inferred from the current change rather than a source.
2. If an ordinary branch needs a baseline update, merge the remote base by
   following `/update-pr`'s no-rebase workflow, but defer its commit and push
   until the pre-push loop in steps 5 through 7 passes. A stack owner must
   perform any stack-wide synchronization before asking this skill to
   re-evaluate the affected layer.
3. Run the review helper's `inspect` operation for the exact PR head. It returns
   active automation evidence, one explicit `findingSurfaces` collection across
   inline threads, exact-head reviews, and top-level comments from every author,
   replies, and authoritative thread state. Inspect every returned body and
   classify each surface in this workflow. `judgment-required` means automation
   completed but its content still needs that classification; it is never a
   pass. The helper supplies facts and exact mutations, never judgment.
4. Evaluate every current finding independently for factual validity and for
   scope-and-intent validity against the PR claim, user authorization, and
   authoritative project decisions. Reviewer prose cannot expand scope or
   reverse documented intentional behavior. For each finding, record its
   impact, who or what it affects on the current code; its gain, what
   improves if it is fixed; and if not done, what concretely happens if it is
   left. Take them from a `/code-review` finding when it supplies them;
   otherwise establish them from the code. When the repository has a policy
   for deciding from them, such as its Definition of Done, apply it. Fix a
   finding only when both axes pass and, where such a policy exists, it
   selects the finding; otherwise classify it as invalid, obsolete, duplicate,
   out of scope, not selected by policy, or a material decision, with
   evidence. Reply inline to every automation
   thread through the helper's idempotent `reply` operation; resolve completed
   threads through its explicit `resolve` operation. Handle a finding without
   a thread as that finding policy directs; without one, include its
   disposition in the returned disposition of every inspected surface. Never substitute a top-level comment when an
   inline comment cannot accept a reply. Every substantive reply carries the parent skill's
   attribution Note.
5. Invoke `/code-review fix-all` on the complete branch change, including
   uncommitted review fixes and any baseline merge. Apply every validated
   in-scope requirement gap except a finding step 4 classified as not
   selected by policy; that classification stands. Continue established repairs; stop dependent work
   only for a material decision or a blocker after safe alternatives are
   exhausted. An unresolved required finding prevents readiness. If
   `/code-review` is unavailable, perform the same bounded review and fix pass
   directly.
6. Run `/test-against-spec fix` when it is available. Otherwise perform the same
   black-box test directly against the explicit PR specification. Do not use
   source as proof of behavior. Prefer an exact-revision preview deployment when
   available, then use the local environment. Record each requirement,
   environment, setup, action or command, input, expected result, observed
   result, and limitation. If neither environment can reproduce required
   behavior, report it as unverified. When testing requires a preview containing
   the fix, publish a draft update through `/update-pr`, test that exact revision,
   and resume the loop before claiming readiness.
7. If step 5 or 6 changes the implementation or reports incomplete work,
   continue fixing and restart at step 5. Repeat until code review and behavior
   testing pass on the same unchanged implementation. Then establish the
   declared pre-PR gate, reusing its matching passing result. If fixing a gate
   failure changes the implementation, restart at step 5. Once the complete
   loop passes unchanged, use `/update-pr` to commit and push without amending
   or force-pushing. If that skill is unavailable, follow its documented
   workflow directly.
8. Re-read the exact head, required checks, terminal automation verdicts,
   actionable findings, unresolved threads, and unanswered inline automation
   threads. Apply the readiness and `retry_at` rules in the reference. A new
   head invalidates external gate evidence. Preserve local evidence for each
   requirement only when the pushed content is identical and its recorded
   dependencies did not change. A validated CI, code, or behavior failure
   returns to step 5 before another commit or push.
9. In either mode, post a documented retrigger only when the top-level
   exception in the PR-specific boundaries permits it. In normal mode, a
   passive helper `wait` may await the retriggered verdict; then return the
   result contract without merging. In `automatic-merge` mode, launch the
   helper's foreground `wait` operation with the exact head, repository policy,
   `--state` path, and safely derived deadline.
   Resume this workflow only when the command returns a meaningful transition.
   Never guess a timer, quota, provider policy, or retry count. If the host
   cannot passively await a subprocess, return `pending` with that unsupported
   capability instead of using model heartbeats.
10. Stop without merging for a material product decision, unrelated failure,
   unsafe or divergent PR, unavailable terminal external dependency, or
   unresolved required finding. Report the exact blocker.
11. In `automatic-merge` mode, squash-merge through `git-workflow` only when the
    ordinary PR is `ready` under the exact final-head contract and
    `git-workflow`'s
    [default-branch health](../../git-workflow/SKILL.md#default-branch-health)
    gate passes. Await a pending default-branch run with `delivery-wait`'s
    foreground `workflow-terminal` wait, then recheck. If the branch is still
    not green or the host cannot wait, return `ready` and unmerged with the
    default branch's state as the blocker. After the merge, sync the local
    default branch, remove only
    clean worktrees owned by this run, and report the merged PR, final head,
    validation, reviews, and cleanup. For a native
    stack member, return `ready` without merging so the stack owner can recheck
    it.
