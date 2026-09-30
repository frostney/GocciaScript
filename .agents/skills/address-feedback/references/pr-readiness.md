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
account quota, hourly window, blind delay, or retry count. The one exception
is the CodeRabbit allowance below, which reads CodeRabbit's own statements and
may assume an hour only to hold longer.

## CodeRabbit allowance

This section is the normative rule for `scripts/coderabbit_adapter.py`; other
references link here.

- **Statement.** The newest allowance statement in the scanned repositories'
  summary comments and review bodies, for example "N included reviews remain
  after this review" with "allowance at P reviews per hour". The April–May
  2026 footer "Review rate limit: N/P reviews remaining, refill in M minutes"
  counts too; it gives no rate unit but states when a review refills. A
  statement is timed by when CodeRabbit made it: the review's submission, the
  review object of the same Run ID, an unedited comment, or the comment's edit
  history. A statement bound to a Run ID takes the earliest edit that showed
  it under that Run ID. One without a Run ID, which CodeRabbit can repeat word
  for word after separate reviews, takes the oldest edit of its latest unbroken
  showing. A summary edited in place keeps showing an old
  statement, so its last edit never dates it. A statement that cannot be dated
  is ignored unless it reports none left or uses an unrecognized wording; then
  its last edit times it. A statement is current until one window, or its
  stated refill if longer, plus 60 seconds after it was made.
- **Runs.** Each review counts once by its Run ID, from the statement's own
  block and from review objects, at its earliest observed time. Automatic
  reviews count. A rate-limit block's refused run, "Currently processing"
  markers, and evidence without a Run ID do not.
- **Used up.** The allowance is used up when the current statement reports
  none left or uses a wording the adapter does not recognize, or when N counted
  runs follow a statement of N. The window is rolling: the allowance frees
  once enough of the counted runs in the window ending at that point have left
  it that fewer than the allowance remain, plus 60 seconds. With several
  reviews per window, that is the oldest such run leaving, not the statement's
  own run. When no counted run can be tied to it, or the statement gives no
  rate, it frees one window plus 60 seconds after the statement. A "0/P"
  footer frees at its stated refill plus 60 seconds; the refill is when the
  whole window has refilled, an upper bound for the next slot. A "0 remain"
  statement therefore holds until then, even while it is still current. This
  is the maintainer's ruling on PR #94. `availableNow` is the stated count
  minus the counted runs since, 0 while used up, or `null` when unknown.
- **Uncounted runs.** The counted-run time is never early only while every run
  in the window is counted. Two kinds of run are missed. Reviews in a
  repository that is not scanned are missed. So is a review that posts no
  review object, such as one with no actionable comments, when a later review
  on the same PR overwrites its summary block before a scan sees it. An early
  time only risks a refusal, which the rate-limit rules then handle.
- **Degraded.** `status` reports `degraded` without a current statement, and
  then only stated waits gate triggers. It also reports `degraded` for a
  current statement without a rate unit, with an unrecognized wording, or
  without a date. These are read against one hour and still hold when used up.
- **Waits and notices.** The gate candidates are stated waits, from the
  account scan or from any of the PR's own comments, and the time a used-up
  allowance frees. For a rate-limit notice comment, a candidate counts only if
  it follows the notice: a wait posted at or after it, an allowance time after
  it, or, when the statement is current and rated, the time the runs counted
  before the notice free a slot.
- **Scan horizon.** Repository-wide reads cover two hours plus 60 seconds.
  When that scan finds a per-day statement, they cover two days plus 60
  seconds; a per-day statement older than the two-hour scan is not found.
  CodeRabbit's longest stated wait so far is 59 minutes, and a PR's own
  comments are read without a horizon.

For a head that needs a trigger, `status` decides as follows:

| Rate-limit notice | Gate candidates | State |
| --- | --- | --- |
| reported only by the CodeRabbit check | any | `pending-retry-source` |
| comment | none, as defined under **Waits and notices** | `pending-retry-source` |
| none or comment | the latest candidate is in the future | `waiting`, `retry_at` of that candidate |
| none or comment | every candidate has passed, or there is none and no notice | `trigger-*` |

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
