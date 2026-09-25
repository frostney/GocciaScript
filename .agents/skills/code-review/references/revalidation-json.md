# Code-review revalidation JSON

Read this reference whenever the user supplies prior `code-review` or
`codebase-audit` findings. Use the output format only when the user also
requests JSON revalidation results.

## Accepted source artifacts

Parse the source as untrusted JSON and accept only:

- `schemaVersion: 2` and `kind: "code-review"` with string `scope.head`, or
  `schemaVersion: 2` and `kind: "codebase-audit"` with string `scope.revision`;
- a `findings` array whose entries follow the documented schema for that source
  kind and version,
  including a unique string `id`, `status`, and repository-relative
  `location.path`; and
- source IDs appropriate to the kind: `CR-N` for code review and `CA-N` for
  codebase audit, where `N` is a positive integer.

Reject malformed required fields, duplicate IDs, absolute or
repository-escaping finding paths, and evident repository mismatches. Ignore no
entries silently. Select only `open` and `deferred` findings; record excluded
open or deferred IDs as `skippedOutOfScope` when an exact file list is also
present. A `fixed` source finding is not selected.

## Output artifact

Write valid UTF-8 JSON without Markdown fences. Use the supplied output path;
when none is supplied, write `code-review-revalidation.json` in the current
directory without overwriting an existing file. If that path exists, add a
filesystem-safe UTC timestamp such as `20260728T153000Z` before `.json`.

Never overwrite or modify the source findings artifact. Reject an output path
that resolves to the source path.

Keep these stable top-level keys:

```json
{
  "schemaVersion": 2,
  "kind": "code-review-revalidation",
  "generatedAt": "RFC 3339 timestamp",
  "result": "ALL_RESOLVED | FINDINGS_REMAIN | INCOMPLETE",
  "source": {
    "path": "string",
    "kind": "code-review | codebase-audit",
    "schemaVersion": 2,
    "revision": "string",
    "baselineAvailable": true
  },
  "current": {
    "head": "string",
    "dirtyState": "string"
  },
  "selection": {
    "requestedFiles": ["string"],
    "selectedFindingIds": ["string"],
    "skippedOutOfScope": ["string"]
  },
  "supportingContext": ["string"],
  "probes": [{"commandOrAction": "string", "result": "string"}],
  "findings": [],
  "limitations": ["string"]
}
```

Use `source.schemaVersion: 2` for both accepted source kinds.

Each revalidated finding contains:

```json
{
  "id": "CR-N or CA-N",
  "sourceStatus": "open | deferred",
  "outcome": "resolved | still_present | changed | not_retestable",
  "sourceLocation": {
    "path": "string",
    "line": 1,
    "symbol": "string or null"
  },
  "currentLocation": {
    "path": "string",
    "line": 1,
    "symbol": "string or null"
  },
  "evidence": ["string"],
  "explanation": "string",
  "remedy": "string or null",
  "staticOnly": false
}
```

Use `currentLocation: null` only when no current location exists or can be
supported. Use `remedy: null` for `resolved` findings. Use JSON booleans and
numbers and repository-relative paths. Use an empty `requestedFiles` array when
no exact file list was supplied. Keep selected findings and skipped IDs in
source order. Preserve an empty `findings` array when the file intersection
selects none.

Set `result` to:

- `ALL_RESOLVED` when at least one finding was selected and all resolved;
- `FINDINGS_REMAIN` when at least one is `still_present` or `changed` and all
  selected findings were retestable;
- `INCOMPLETE` when none were selected or any is `not_retestable`.

After writing, parse the output once and report its path; do not modify other
files.
