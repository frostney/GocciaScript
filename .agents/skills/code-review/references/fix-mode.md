# Fix follow-up

Read this when the user selects `fix <finding IDs>` or `fix-all`. The
authorization and stop boundaries in `SKILL.md` still apply.

- Implement the smallest remedies without expanding the agreed change. The
  coordinator makes every edit.
- Promote a useful repro into a regression test; otherwise remove it.
- Rerun affected behavioral probes and project checks once after the fixes, then
  report fixed and unresolved IDs with observed results. Do not start an
  unbounded review-fix-review loop.
- Do not redispatch completed lanes after fixes; re-engage a worker only to
  resolve incomplete or contradictory evidence.
- With prior-findings input, keep remediation within the eligible findings
  defined in `SKILL.md` and do not turn it into a fresh review.

Return fixed and unresolved findings to the caller, which owns the development
or delivery loop. Record deferred optional improvements separately; they do not
extend the agreed work.
