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
- every intentionally active review automation has a terminal completed verdict
  explicitly tied to that head, with no newer incomplete or follow-up review;
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
review is not a completed verdict.

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
orchestrator. Its foreground wait stays silent while unchanged and returns only
when review becomes ready, evidence changes materially, the expected head is
invalidated, the deadline arrives, or an operational failure needs attention.

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
unambiguously, or do not clearly describe availability. Never infer a provider,
account quota, hourly window, blind delay, or retry count. CodeRabbit's
retry times come only from its adapter; see [CodeRabbit](#coderabbit).

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

| State | Reported by | Workflow |
| --- | --- | --- |
| `review-complete` | `run`, `status` | Classify every finding of the head's CodeRabbit review. |
| `clean-complete` | `run`, `status` | CodeRabbit is complete for the head with no findings. |
| `satisfied` | `status` | Every head is complete. |
| `trigger-incremental`, `trigger-full` | `run`, `status` | Pending: `run`, which posts it. |
| `awaiting-automatic`, `triggered`, `in-progress`, `waiting` | `run`, `status` | Pending: `run` waits; after its deadline, `run` again. |
| `rate-limited-unknown-wait` | `run`, `status` | Pending: `run` again later; never trigger by hand. |
| `pending` | `run`, `status` | A head is pending, or GitHub was unreachable until the deadline: `run` again. |
| `lock-held` | `run` | Pending: another `run` holds the trigger lock; `run` again later. |
| `draft` | `run`, `status` | Mark the PR ready when the workflow permits, then `run`. |
| `blocked` | `status` | Act on each blocked head's state. |
| `skipped` | `run`, `status` | Blocked: report the reason; a person must act. |
| `paused` | `run`, `status` | Blocked: CodeRabbit paused its reviews of the PR. A person resumes them or triggers a review. |
| `blocked-unanswered` | `run`, `status` | Blocked: CodeRabbit did not answer the trigger. A person checks CodeRabbit on the PR, then triggers again or pushes. |
| `blocked-stalled` | `run`, `status` | Blocked: CodeRabbit's review never finished. A person checks CodeRabbit on the PR, then triggers again or pushes. |
| `blocked-unknown-wait` | `run`, `status` | Blocked: CodeRabbit refused the head without a stated wait. A person reads its notice on the PR and triggers when it allows. |
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
