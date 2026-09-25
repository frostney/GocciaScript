# Address stack feedback

Converge review feedback for exactly one native GitHub stack. Review every
initial layer at its exact head, preserve those frozen heads, collect validated
live fixes in one new top layer per round, and stop only when the complete live
stack reaches a clean fixed point or an explicit blocker.

Apply the parent SKILL.md shared authority, attribution, and evidence rules.
Require one repository-scoped native stack number verified through the Stacks
API or `gh stack`. A finding is live only when true in the integrated stack-top
tree and within its confirmed claim. Never mutate a frozen layer; use only
guarded official stack operations allowed by `git-workflow`.

Read [stack-readiness.md](stack-readiness.md) before assigning layer or
stack state.

Use `scripts/stack_state.py` with `--json` for native topology snapshots and
change waits. Use `scripts/review_wait.py` with `--json` for
each pull request's finding surfaces, review transitions, replies, and
resolutions. The helpers supply facts and exact mutations; this workflow owns
finding judgment, remediation, coverage, and readiness.

Without a valid review policy, `inspect` still returns feedback and check facts
but marks automation requirements unknown. Use these facts for discovery;
neither missing policy nor an empty finding list establishes completion. Supply
a verified policy before using `wait`.

The helper completes pagination and rejects changing feedback censuses. Its
automation state accounts for newer incomplete attempts and distinguishes
reply-only review records from verdicts. See the deterministic mechanism in
[pr-readiness.md](pr-readiness.md) when interpreting those helper states; the
complete-stack gate remains in `stack-readiness.md`.

When current policy or activity identifies CodeRabbit as active, use
`scripts/coderabbit_adapter.py`; do not reconstruct its trigger, completion,
acknowledgment, coverage, rate-limit, or locking logic from prose. In read-only
mode, run `status` with every exact `PR=SHA`. In normal mode, run `run` for one
PR at a time with its exact head and an absolute deadline. Supply every known
repository with recent CodeRabbit activity through repeated `--scan-repo`
arguments so the adapter can select the newest edited account-scoped wait.

## Review rounds

1. Confirm a clean worktree for normal mode, fetch the remote default, read the
   native-stack rules, verify `gh stack` authentication, and inspect the exact
   stack number. In read-only mode, inspect remotely without checking out the
   stack. Capture the bottom-to-top members, base, drafts, exact heads, current
   checks, review policy, provider activity, threads, and finding surfaces.
2. Establish the complete stack claim from its pull-request bodies, linked
   requirements, project contracts, and current integrated top. Mark inferred
   claims. Stop when member scope is unrelated or the stack cannot safely ship
   as one atomic unit.
3. Discover active review providers from current policy, checks, and activity.
   Keep the core provider-neutral. Use executable adapter code only for behavior
   that generic GitHub checks, reviews, comments, and explicit retry times cannot
   represent. A missing or ambiguous required provider operation is pending,
   never a guessed trigger, completion, or wait.
4. Review every initial member once for its exact head. Serialize triggers when
   the active provider has account-wide, repository-wide, or other shared
   limits. During waits, use foreground transition commands and passively await
   them. Do not wake the model merely to report unchanged state.
5. Inspect every inline thread, exact-head review body, and top-level
   finding surface. Classify each claim against the integrated stack top:
   `moot`, `satisfied-later`, `mutated`, `live`, `declined`, or
   `material-decision`. Cite the exact descendant commit and call path for
   `satisfied-later`; do not accept a vague later-layer claim.
6. Reply and resolve a moot, satisfied-later, or evidence-backed decline only
   after its disposition is supported and accepted under repository policy. A
   decline requiring human judgment remains unresolved and blocks readiness.
   Record every live finding with its originating PR and thread or surface.
7. If live findings remain, check out the current native stack, record every
   remote head, and create exactly one new branch above the current top with
   `gh stack add`. Fix all validated live findings there. Group commits by one
   coherent cause or originating PR where that preserves traceability; every
   source reply must link the exact landing commit and fix-layer PR. For a
   documentation finding, search the document and any file that mirrors the
   same fact for sibling statements, and fix them in the same commit; an
   incremental review surfaces each missed sibling one round later.
8. Run focused validation while fixing. Before submitting a substantive fix
   layer, repeat `/code-review fix-all` and `/test-against-spec fix` against the
   complete integrated tree until both pass on the same unchanged content, then
   establish the project's declared gate using matching current results. Stop for a material product, architecture,
   security, compatibility, or scope decision.
   If required behavior can only be exercised on a PR preview, complete the
   available checks and use step 9 to publish the necessary draft fix layer,
   explicitly retaining the missing preview evidence. Test its exact revision
   and resume this loop before readiness. Further repairs still respect frozen
   members; append a new fix layer when the published member is already frozen.
9. Enforce the native-stack reference's push-boundary checks for the frozen
   prefix and new branch. Use its protected `gh stack push` followed by separate
   PR creation and native append procedure; preserve the frozen PR heads and
   metadata. Reconcile a partial result before continuing. Immediately replace generated
   metadata with its actual claim, validation, and originating finding links,
   and satisfy the project's PR evidence contract. Keep it draft until its own
   local evidence and required checks permit review. Trigger and collect its
   exact-head review under the active provider contract.
10. Once that fix layer has successful exact-head CI and a terminal review, it
    is frozen. Reply to and resolve the originating threads with the exact fix
    evidence. If review of the fix layer produces live findings, create another
    top layer; never push those fixes into any frozen member.
11. After every head, topology, review, or thread transition, reconcile native
    membership through the stack helper. An expected append-only fix layer keeps
    the frozen prefix valid but needs its own review. An unexpected head or
    topology change invalidates the affected evidence under the readiness
    reference and restarts the required portion of the round.
12. Before reporting readiness, capture the live stack again and compare it with
    the snapshot used by the final evidence pass. Require every current member
    to be present in the evidence set and the newest fix layer to be reviewed
    clean. A drained trigger list is not completion when live membership differs.

## Provider adapters

Provider-neutral behavior is the default. A provider adapter may define only
the provider-specific trigger, completion evidence, contention scope, explicit
retry source, and escalation needed to obtain a trustworthy review. It does not
own stack identity, findings, fixes, readiness, merge authority, or general CI.

Do not invent adapters for ordinary GitHub review state. Do not copy provider
commands, comment parsing, timers, or paid options into the core workflow. The
CodeRabbit adapter is the sole owner of its two permitted trigger commands. Its
`run` operation serializes triggers with one authenticated-account lock, polls
through stated waits, and treats a finished acknowledgment as `clean-complete`
only when walkthrough coverage is verified for the exact head and either ack
latency meets the trusted threshold or that same head has a CodeRabbit check
SUCCESS. It escalates untrusted incremental acknowledgments to a full review,
refuses guessed retry times, and never exposes a paid-review command. Keep
`review-complete` for real exact-head review objects only.

## Result contract

Return:

- repository and native stack number;
- final bottom-to-top PR, branch, and exact-head list;
- each member's `reviewed`, `covered`, `pending`, or `blocked` state;
- every finding surface and its disposition, source thread, and exact fix or
  decline evidence;
- each created fix layer and its validation, CI, and review result;
- current unresolved and unanswered thread counts;
- active provider state and safely derived `retry_at` or `null`;
- topology changes and the evidence they invalidated;
- complete-stack state: `ready`, `pending`, or `blocked`; and
- blocker or next required transition.

Never report a partial prefix as ready when any required fix exists only in a
higher layer. A `ready` result never means merged.
