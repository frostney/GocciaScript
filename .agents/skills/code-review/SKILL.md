---
name: code-review
description: >-
  Review a PR, branch, or worktree for evidence-backed findings. Supports scoped
  revalidation and explicitly requested fixes or review workers.
license: Unlicense OR MIT
compatibility: >-
  Requires git, the project's declared build and test tools, and network access
  when pull-request context or current third-party documentation is relevant.
---

# Code review

Establish whether the requested review scope is correct, necessary, clear, and
ready for its claimed use. Without an explicit file or prior-findings input,
review the complete change. Review first; remediate only in an authorized fix
mode.

## Operations, remediation, and boundaries

Choose one operation before gathering evidence:

- **Fresh review:** judge the bounded change and issue a review verdict.
- **Targeted revalidation:** recheck selected prior findings without judging the
  change as a whole.
- **Combined:** perform both only when the user explicitly requests a fresh
  review and supplies prior findings; keep their outputs and conclusions
  separate.

Remediation is independent of the operation: default is read-only, `fix
<finding IDs>` fixes only selected findings, and `fix-all` fixes every validated
in-scope finding in one bounded pass. Stop remediation for a material product,
architecture, security, compatibility, or scope decision.

- Exact file lists and prior-findings JSON are additive inputs. They do not
  change unscoped review behavior unless the user supplies them.
- A reporting profile or threshold changes presentation only. Gather and
  validate the complete candidate set, retain every supported severity in the
  canonical result, and let the caller decide which severities become visible.
- `subagents` is an additive execution input. Without it, do not delegate any
  part of the review.
- Default remediation is none. Inspect and run safe local probes, but do not
  edit source, tests, configuration, or documentation.
- Fix modes authorize local edits and validation, not commits, pushes, PR
  comments, review-thread changes, deployments, publication, or shared-state
  mutation.

Safe probes include declared checks, local builds and servers, disposable
repros, isolated test data, browser interaction, temporary artifacts, and
revert-clean falsification probes. A falsification probe temporarily introduces
one targeted wrong behavior to prove the relevant test or gate fails for the
right reason. Record the initial tree state, prefer a disposable worktree or
copy, restore the mutation immediately, compare the final tree byte-for-byte
with the recorded state, and report the mutation and observed failure. Skip the
probe and mark the evidence static-only when exact restoration is not safe.
Clean up disposable artifacts and report retained ones. Ask before any
persistent or externally visible side effect.

A request to save JSON authorizes only the named findings artifact in default
mode; it does not authorize remediation. For ordinary review findings, read
[references/findings-json.md](references/findings-json.md) only when JSON output
is requested. For targeted revalidation, read
[references/revalidation-json.md](references/revalidation-json.md) whenever
prior findings are supplied, whether or not JSON output is requested.

## Additive inputs

Read only the references for supplied inputs:

- [references/subagent-lanes.md](references/subagent-lanes.md) for `subagents`.
- [references/file-scope.md](references/file-scope.md) for an exact file list.
- [references/prior-findings.md](references/prior-findings.md) for prior findings,
  their intersection with a file list, and the targeted result contract.

## Establish a fresh review

Use this section for a normal review or when the user explicitly combines
revalidation with a fresh review. For targeted revalidation alone, use the
source selection and recorded baseline above and gather evidence only for the
selected prior findings.

1. Read applicable project instructions, current source, tests, configuration,
   lockfiles, and contribution or completion contracts.
2. Resolve the comparison boundary:
   - use the user-supplied base when present;
   - for a pull request, use its base branch;
   - otherwise use the merge-base with the remote default branch.
3. Resolve the fixed point and head to concrete revisions before delegation,
   then verify that the bounded diff can be computed and is non-empty. Stop
   before review when either revision is unavailable, the boundary is ambiguous,
   or the change is empty or unrelated.
4. Include committed, staged, unstaged, and relevant untracked work. Separate
   dirty-worktree findings from committed-change findings.
5. Establish the claim from the issue, PR, confirmed mini-spec, required
   behavior, and commits. If none exists, reconstruct the narrowest supported
   claim from the change and label it as inferred.
6. Activate de-duplication, claim and specification, and engineering quality for
   every fresh review. Activate discoverability only for changes to public pages,
   routing, metadata, crawl controls, structured data, public content, or
   web-performance behavior. Within engineering quality, cover correctness,
   simplification, self-documentation, test value, and operational behavior;
   add UI/accessibility, trust boundaries, persistence/migrations, concurrency,
   compatibility, deployment/rollback, observability, or performance only when
   the change touches those concerns.
7. When the change touches authentication, authorization, payments, secrets,
   destructive or data-loss behavior, or tenant isolation, read
   [references/adversarial-review.md](references/adversarial-review.md) and apply
   its bounded bypass hunt within engineering quality.
8. When structural evidence suggests a design smell but concrete impact or the
   smallest remedy is unclear, read
   [references/engineering-smells.md](references/engineering-smells.md) as
   optional investigation prompts. Repository standards and observed impact
   remain authoritative.
9. Measure churn for every changed file in the finding scope and, where history
   can identify it reliably, each changed function, method, class, or module.
   Follow renames, state the history window, and record touch count and line
   churn. Use the repository's declared churn window or 90 days when none
   exists. Prefer its code-health tool; otherwise use Git file history and
   `git log -L` for stable symbols. Label file-level fallback when symbol
   history is unavailable.

## Generate evidence

For a fresh review, apply these requirements across the mapped finding scope.
For targeted revalidation, apply them only where they test a selected prior
finding.

- Establish the repository's relevant gate from current evidence or run the
  missing checks. In a composed workflow the caller owns the aggregate gate.
  Reuse passing checks and real-interface evidence for matching content, command,
  environment, and coverage; rerun after changes, failures, gaps, or unresolved
  concerns. Preserve independent review judgment. Do not restate clear tooling
  failures.
- Reproduce each changed observable behavior through the real interface. Cover
  the intended path and the most consequential failure or boundary path.
- For UI changes, exercise the rendered interface, state transitions,
  loading/empty/error states, accessibility, and relevant viewports. For
  non-UI changes, exercise the real API, CLI, library entry point, job,
  migration, packaging, or deployment path.
- Record setup, action or command, input, expected result, and observed result.
  Credit returned results or matching stored evidence; a request, acknowledgment,
  or expected outcome is not an observed result. Mark missing results
  `unverified` and source-only conclusions `static only`.
- Verify that changed tests fail for the relevant wrong behavior and assert
  outcomes rather than implementation details. Do not credit brittle,
  over-mocked, incidental, or snapshot-heavy coverage.

## Review axes

Keep the axes distinct so one cannot mask the other.

### De-duplication

Apply four separate checks across the bounded change and its minimum supporting
context:

- **Implementation:** find repeated code, logic, tests, fixtures, configuration,
  schemas, workflows, documentation, or competing representations of one
  concept.
- **Work:** reuse current issue decisions, prior findings, investigations, and
  accepted remediation evidence instead of repeating them. Revalidate rather
  than rediscover when their scope overlaps the change.
- **Evidence:** coalesce the same event reported by multiple checks, logs, or
  tools so it is counted once while retaining every source.
- **Output:** combine candidates with the same cause, impact, and remedy into one
  finding, preserve provenance, and explicitly reconcile contradictory evidence.

Do not expand finding scope beyond the bounded change. Duplication visible only
in supporting context can support an in-scope finding but is not a separate
finding there.

### Claim and specification

Find missing or partial requirements, incorrect behavior, and unrequested scope.
Cite the originating requirement or identify the claim as inferred.

### Engineering quality

- Trace changed inputs, authorization, state transitions, failures, retries,
  concurrency, idempotency, deletions, and side effects where relevant.
- Search the live repository before accepting new helpers, patterns, formats, or
  abstractions. A second representation or implementation of the same concept
  is a defect unless the repository documents why it exists.
- Prefer deletion, reuse, direct control flow, and existing dependencies. Report
  dead paths, duplication, speculative layers, needless wrappers, one-use
  indirection, and custom code already provided by the platform or dependencies.
- Require names, types, boundaries, and interfaces to reveal intent. Comments
  should explain rationale, constraints, or non-obvious behavior rather than
  translate the code; surrounding comment density is not a requirement.
- Treat repeated changes to the same symbol or file as an architectural-risk
  signal, not a defect by itself. Raise an `ARCHITECTURE_RISK` finding when the
  measured churn coincides with mixed responsibilities, recurring fixes or
  reverts, competing representations, broad blast radius, unstable interfaces,
  or weak regression coverage. Cite the window, touch count, granularity, and
  co-signal.
- Treat generic best practice and remembered library behavior as leads only.
  Verify findings against the checked-out code, exact installed version, and
  current official documentation or source. Repository decisions override
  generic preferences.

### Discoverability

For an active public-web surface, verify crawl and index controls, canonical and
descriptive metadata, internal discovery paths, structured data that matches
visible content, semantic content structure, rendering, and material web
performance. Assess conventional search and AI-assisted discovery together,
while keeping crawler access, search inclusion, and model-training controls
distinct. Use current official search-engine and publisher guidance; do not
invent special AEO markup, keywords, or guarantees.

## Fresh-review report

For a fresh review, lead with the verdict: `APPROVE`,
`APPROVE WITH IMPROVEMENTS`, or `REQUEST CHANGES`.

Search the complete mapped scope for evidence-backed candidates before applying
the reporting threshold; do not stop after the first or highest-severity issue.

Include:

- the claim, comparison boundary, commits and dirty state reviewed;
- active and skipped review axes, with the reason for each skip;
- the material engineering-quality concerns covered and any conditional concern
  skipped because the changed runtime path did not touch it;
- when `subagents` was supplied, the review-axis-to-lane map, completed and
  incomplete lanes, and every coordinator-completed fallback with its reason;
- the churn window, symbol/file coverage, and architectural-risk hotspots;
- exact probes and checks with observed results;
- de-duplication coverage, coalesced evidence sources, and merged or conflicted
  candidate findings;
- actionable findings as
  `[CR-N][BLOCKING|IMPORTANT|IMPROVEMENT|NITPICK][CLAIM|QUALITY|ARCHITECTURE_RISK|
  DISCOVERABILITY]
  file:line: evidence, impact, smallest remedy`;
- verified claims, static-only or unreached areas, and retained probe artifacts.

Render every literal repository path, filename including extensionless files,
variable, function, method, class, type, and other code identifier as inline
code. Keep prose outside code spans.

A remedy that tells the author how to update, commit, or publish the branch
names the mechanism the repository documents, not a generic one. Check the
project's git workflow first: where it forbids rebasing an ordinary branch,
write "merge the base branch in", and reserve stack commands for branches in a
confirmed stack. The same applies to review comments and pull request text
posted from the report.

`BLOCKING` prevents safe shipment. `IMPORTANT` has material correctness,
security, operability, test-value, maintainability, simplification, or
comprehension cost. `IMPROVEMENT` is a verified worthwhile simplification or
current-practice alignment. `NITPICK` is a small, local polish issue with a
clear remedy and evidence from repository conventions or current code; it must
not represent personal taste. Optional polish does not block readiness, but a
verified requirement gap cannot be waived by assigning it a lower severity.
Omit praise, diff narration,
subjective style preferences, and findings without concrete impact.

## Fix follow-up

In a fix mode, implement the smallest remedies without expanding the agreed
change. Promote a useful repro into a regression test; otherwise remove it.
Rerun affected behavioral probes and project checks once after the fixes, then
report fixed and unresolved IDs plus observed results. Do not start an
unbounded review-fix-review loop. The coordinator makes every edit. Do not
redispatch completed lanes after fixes; re-engage a worker only to resolve
incomplete or contradictory evidence.

For prior-findings input, default to read-only revalidation. An explicit
`fix <finding IDs>` may remediate only matching selected findings classified
`still_present` or `changed`; `fix-all` may remediate all such selected
findings. Never edit for `resolved`, `not_retestable`, or `skippedOutOfScope`
findings, and do not turn remediation into a fresh review.

Return fixed and unresolved findings to the caller, which owns the development
or delivery loop. Unresolved `BLOCKING` or `IMPORTANT` findings prevent readiness.
Every verified gap against the agreed requirements must be resolved regardless
of severity. Record deferred optional improvements separately; they do not
extend the agreed work.
