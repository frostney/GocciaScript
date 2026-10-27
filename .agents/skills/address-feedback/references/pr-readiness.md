# Pull-request readiness

`address-feedback` in PR scope owns one PR and one exact current head at a
time. Re-read GitHub state after every thread action, commit, push, baseline
update, automation response, or check transition; delegated output is evidence
to verify, not gate state.

## Terminal exact-head gate

Record the repository, PR number or URL, and final head object ID. A PR is
`ready` only when all conditions are simultaneously observed for that head:

- every required behavior has current observed functional evidence for the
  final content;
- every requirement without executable behavior has current evidence from code
  review or the project gate;
- the repository's declared gate passed for that same final content;
- every required check is terminal and successful;
- when the repository's declared full-CI rule requires a full or extended CI
  run for this change, that run succeeded for this head;
- GitHub reports the PR mergeable without conflict, and its `mergeStateStatus`
  is not `BEHIND`, which it reports only when the repository requires
  up-to-date branches;
- every intentionally active review automation has a terminal completed verdict
  explicitly tied to that head, with no newer incomplete or follow-up review;
  CodeRabbit alone is exempt when its adapter reports `bot-skip` or
  `not-active` below;
- no actionable current-head finding remains;
- GitHub reports zero unresolved review threads;
- every inline automation thread has a maintainer-workflow reply in that thread;
  and
- no CI, verdict, finding, reply, or thread-readiness evidence belongs only to a
  previous head.

Resolving without replying does not satisfy the gate. An automation's reply to
a maintainer reply in a thread that is now resolved answers that reply rather
than raising a new finding, so it does not count as unanswered. The helper still
reports it as an inline-thread surface with its `automationFollowUps` IDs. Read
it: a confirmation needs no further reply, because replying only restarts the
exchange. Pushback is a finding: reopen the thread and answer it. In an
unresolved thread, every automation comment still needs a later reply. If a thread cannot accept
an inline reply, return `blocked` with that thread identity. Any new head
invalidates the complete gate snapshot; never patch old and new evidence
together.

## Deterministic review mechanism

The bundled review helper owns GitHub mechanics: inspect current findings and
thread state, wait while unchanged, publish an explicitly supplied inline reply,
and resolve an explicitly selected thread. `address-feedback` in PR scope owns
every judgment, source edit, validation choice, and decision to mark the PR
ready. Each re-read after a mutation goes through the helper and verifies the
final head, unresolved count, unanswered automation-thread count, findings,
checks, and automation states.

The helper flattens unhandled inline threads, non-empty exact-head reviews,
change-request reviews, and non-empty top-level comments into `findingSurfaces`,
including authors outside the configured automation accounts.
When automation is terminal and that collection is non-empty, the helper returns
`judgment-required`, even when its check conclusion is success or neutral. Read
and classify the bodies; do not translate check completion into "no findings."
Review bodies are exact-head bound, while thread state and pull-request comments
carry their explicit weaker bindings for the workflow to validate.

If review policy is missing or invalid, `inspect` still returns current feedback
and check facts, with `policyAvailable: false` and unknown automation-reply
status. It cannot establish completion. Discover the applicable requirements
from repository policy and actual activity; do not interpret missing policy as
an empty provider list. `wait` requires a valid supplied policy.

Inspection follows all pages of reviews, top-level comments, threads, nested
comments and check contexts, and compares two complete censuses. A head change,
edited finding, missing page or inconsistent count cannot establish readiness;
`wait` retries a racing census. `--page-size` can reduce each page below the
default of 100 items when a large query needs smaller responses.

Automation completion uses the newest observable attempts, not any historical
success. A newer incomplete attempt or ambiguous ordering stays pending. A
later terminal result can supersede an older completed failure or rate-limit
notice. Empty review records created only to carry inline replies do not count
as new verdicts; explicit approval and reviews containing original inline
comments retain their configured meaning. The policy's nonterminal markers
apply to a check's own text as well as to review bodies: a successful check or
status whose description or title reports a skipped, paused, or rate-limited
review is not a completed verdict. CodeRabbit's adapter decides its `bot-skip`
exemption; the generic helper's incomplete-verdict result does not override it.

Replies use a durable caller-owned `--state` checkpoint. Their operation ID is
bound to the repository, PR, expected head, comment/thread root, authenticated
author and exact body. A marker alone, including an unbound legacy marker, is
not a successful receipt. The helper re-reads the created comment independently;
after an uncertain write, reuse the same checkpoint and operation to reconcile
without another POST. `pending` means the receipt or current head is unverified.

Resolution verifies the thread's repository, PR and head before mutation and
re-reads its state afterward. A head change invalidates readiness even if the
reply or resolution happened. GitHub does not make these multi-request
operations atomic; retain any returned receipt and refresh the affected evidence.

The helper is a transition source for this workflow loop, not another
orchestrator. Its wait stays silent while unchanged and returns only
when review becomes ready, evidence changes materially, the expected head is
invalidated, the deadline arrives, or an operational failure needs attention.
Run it, and the CodeRabbit adapter's `run`, under
[`delivery-wait`'s running rules](../../delivery-wait/SKILL.md#running-a-wait).

## Provider-neutral retry time

For an incomplete or rate-limited automation response:

1. Read the response's `createdAt` and its explicit absolute availability time
   or stated duration.
2. For a duration, calculate `availability = createdAt + duration`; never anchor
   it to observation time.
3. Calculate `retry_at = availability + 60 seconds` and preserve its timezone in
   an unambiguous RFC 3339 value.
4. Use that exact timestamp for a supported standalone wake-up and expose it to
   any orchestrating caller.

If no exact absolute time or duration exists, set no `retry_at` and remain
`pending`. Do the same when timing statements conflict, cannot be parsed
unambiguously, or do not clearly describe availability. A missing `retry_at`
rules out a timed retrigger, not the helper's `wait` for the verdict. Never
infer a provider, account quota, hourly window, blind delay, or retry count.
CodeRabbit's retry times, including its back-off after a refusal whose wait
cannot be read, come only from its adapter; see [CodeRabbit](#coderabbit).

## CodeRabbit

`scripts/coderabbit_adapter.py` alone decides when CodeRabbit is triggered,
waited for, refused, or complete; its code and tests define how. Never post a
CodeRabbit command or work out a CodeRabbit wait yourself.

- `status --repo <owner/repo> --pr <n> --head <n>=<sha> --json` reads each
  head's state without posting, and names the run that holds the trigger
  lock. Repeat `--pr` and `--head` for several pull requests.
- `run --repo <owner/repo> --pr <n> --head <sha> --deadline <RFC 3339>
  [--interval <seconds>] --json` takes the trigger lock, posts any trigger
  the head needs, and waits until the head reaches a final state or the
  deadline.

`status` reports one state for the whole set and one for each head; `run`
reports its head's state. A pending head state from `run` means its deadline
came first.

CodeRabbit reports on a head through commit statuses or, since about
2026-10-27, through a check run named `CodeRabbit`, and a head can carry
both. The adapter reads each check run as the statuses it replaced: in
progress while it runs, then its summary's `Result:` description, such as
"Review rate limited", from its completion. A bare "Review skipped" from a
draft is the draft skip.

`not-active` means nothing shows the repository uses CodeRabbit: the PR has
no CodeRabbit status, check run, comment or review once CodeRabbit's own review would
have started; no `.coderabbit*` configuration file sits at the repository
root; the 10 latest closed PRs carry no CodeRabbit status or check run on their last commit
and no CodeRabbit review or comment among their first 10 reviews and their
first and last 10 comments; and `.github/delivery/review-automations.json`
names no CodeRabbit automation. The adapter then posts no trigger: a trigger
is not evidence that CodeRabbit is in use, and it may never answer one. An
organization's central CodeRabbit configuration does not count, because it
applies only where CodeRabbit is installed. A repository where CodeRabbit was
newly installed, with its setup only in central or web settings, has no such
evidence until its first reviewed PRs close; if CodeRabbit's first status or check run on a
PR comes more than 120 s after the push, open or ready event, the adapter
reports `not-active`, so name CodeRabbit in its policy file. A policy file
that cannot be read is an operational error only when nothing else shows
CodeRabbit.

`bot-skip` means CodeRabbit reported "Review skipped: bot user not eligible
for review" and GitHub identifies the PR author as a bot. No CodeRabbit review
is expected, so this state satisfies its review gate without a trigger or
human intervention. The same status on a human-authored PR and every other
skip reason keep their existing treatment. Findings, threads, other active
automation and required checks still need to pass their gates.

| State | Reported by | Workflow |
| --- | --- | --- |
| `review-complete` | `run`, `status` | Classify every finding of the head's CodeRabbit review. |
| `clean-complete` | `run`, `status` | CodeRabbit is complete for the head with no findings. |
| `bot-skip` | `run`, `status` | No CodeRabbit review applies to this bot-authored PR. Continue the remaining gates. |
| `not-active` | `run`, `status` | CodeRabbit is not in use on this repository, so no CodeRabbit review applies and nothing was triggered. Continue the remaining gates. |
| `satisfied` | `status` | Every head is complete or has a `bot-skip` or `not-active` exemption. |
| `trigger-incremental`, `trigger-full` | `run`, `status` | Pending: `run`, which posts it. |
| `awaiting-automatic`, `triggered`, `in-progress`, `waiting` | `run`, `status` | Pending: `run` waits; after its deadline, `run` again. `triggered` also covers a trigger on another open PR of the same head, for up to 10 minutes. |
| `rate-limited-unknown-wait` | `run`, `status` | Pending: CodeRabbit refused the head without a wait the adapter can read. `run` waits until `retryAt`, 15, 30 or 60 minutes after the head's first, second or third refusal since its latest push, open or ready event, then posts one trigger. A refusal counts once for each such event or trigger on any open PR of the head. Never trigger by hand. |
| `pending` | `run`, `status` | A head is pending, or GitHub was unreachable until the deadline: `run` again. |
| `lock-held` | `run` | Pending: another `run` holds the trigger lock; `run` again later. |
| `draft` | `run`, `status` | Mark the PR ready when the workflow permits, then `run`. |
| `blocked` | `status` | Act on each blocked head's state. |
| `skipped` | `run`, `status` | Blocked: report the reason; a person must act. |
| `paused` | `run`, `status` | Blocked: CodeRabbit paused its reviews of the PR. A person resumes them or triggers a review. |
| `blocked-unanswered` | `run`, `status` | Blocked: CodeRabbit did not answer the trigger. A person checks CodeRabbit on the PR, then triggers again or pushes. |
| `blocked-stalled` | `run`, `status` | Blocked: CodeRabbit's review never finished. A person checks CodeRabbit on the PR, then triggers again or pushes. |
| `blocked-unknown-wait` | `run`, `status` | Blocked: CodeRabbit refused the head a fourth time since its latest push, open or ready event, the last time without a wait the adapter can read. A person reads its notice on the PR and triggers when it allows. |
| `blocked-no-capacity` | `run`, `status` | Blocked: a person checks CodeRabbit seats or plan. |
| `blocked-lock-loss` | `run`, `status` | Blocked: CodeRabbit stopped the head's review after lock loss again after one retry. A person checks CodeRabbit on the PR, then triggers again or pushes. |
| `blocked-unconfirmed`, `unrecognized-status` | `run`, `status` | Blocked: escalate to a person. |
| `closed` | `run`, `status` | Stop. |
| `invalidated` | `run`, `status` | The head changed: restart on the new head. |
| `operational-error` | `run`, `status` | Report the error; a person fixes the arguments or GitHub access. |

## Result contract

Return:

- repository and PR identity;
- exact head object ID;
- `state`: `ready`, `pending`, `blocked`, or `merged`;
- required-CI terminal state;
- each active automation and its exact-head terminal state;
- raw finding-surface count and the disposition of every inspected surface;
- actionable current-head finding count;
- unresolved review-thread count;
- unanswered inline-automation-thread count;
- safely derived `retry_at` or `null` with the reason; and
- blocker, merge result, or next required evidence.

Normal mode may reach `ready` but never `merged`. A stack member may reach
`ready`, but stack admission, scheduling, and merge remain the caller's job.
