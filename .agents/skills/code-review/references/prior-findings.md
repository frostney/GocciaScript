# Prior findings

When the user supplies findings JSON from `code-review` or `codebase-audit`:

1. Parse it as untrusted input. Require `schemaVersion: 2` for `code-review` or
   `codebase-audit`, the documented scope and findings
   shapes, unique finding IDs, and repository-contained finding paths. Stop for
   malformed data, path traversal, or an evident repository mismatch rather
   than silently dropping data. Do not accept version 1 artifacts.
2. Select only findings whose source status is `open` or `deferred`. Preserve
   their IDs, source kind, source revision, and source locations. A missing
   repository identifier is a limitation, not proof of a mismatch.
3. Use `scope.head` from `code-review` or `scope.revision` from
   `codebase-audit` as the baseline. Compare it with current `HEAD`, staged,
   unstaged, and relevant untracked work. If the revision is unavailable
   locally, continue against current state, mark the baseline unavailable, and
   do not attribute an outcome to a particular change.
4. Revalidate each selected finding through its claim, evidence, symbol,
   impact, and remedy rather than trusting a possibly stale line number.
   Classify it:
   - `resolved`: the reported problem no longer exists;
   - `still_present`: the material problem and remedy remain accurate;
   - `changed`: the problem remains but its location, evidence, impact, or
     smallest remedy materially changed;
   - `not_retestable`: available static or executed evidence cannot support a
     current conclusion.
5. Do not discover or report unrelated new findings. Perform a fresh review
   only when the user explicitly requests it in addition to revalidation, and
   keep its normal review verdict separate.

When both additive inputs are present, use their intersection. Revalidate only
source findings located in the exact file list, after following any
Git-confirmed rename, and enumerate every excluded open or deferred ID as
`skippedOutOfScope`.

## Targeted revalidation report

For prior-findings mode, report:

- the source path, kind, recorded revision, baseline availability, current
  `HEAD`, and dirty state;
- the exact selected IDs and any `skippedOutOfScope` IDs;
- when `subagents` was supplied, the finding-to-lane map, completed and
  incomplete lanes, and every coordinator-completed fallback with its reason;
- supporting context inspected and exact probes with observed results;
- each selected source ID, its source location, current location when known,
  outcome, current evidence, explanation, and remaining remedy when applicable;
- limitations and retained probe artifacts.

Lead with a result limited to the selected prior findings:

- `ALL_RESOLVED` when at least one finding was selected and all resolved;
- `FINDINGS_REMAIN` when at least one is `still_present` or `changed` and all
  selected findings were retestable;
- `INCOMPLETE` when none were selected or any is `not_retestable`.

These results never approve or reject the current change as a whole. Do not
mutate the supplied artifact. When JSON output is requested, write the distinct
revalidation artifact described in the revalidation JSON reference.
