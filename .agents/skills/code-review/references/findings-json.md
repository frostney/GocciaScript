# Code-review findings JSON

Use this format only when the user requests JSON output. Write valid UTF-8 JSON
without Markdown fences. Use the supplied path; when none is supplied, write
`code-review-findings.json` in the current directory without overwriting an
existing file. If that path exists, add a filesystem-safe UTC timestamp such as
`20260727T153000Z` before `.json`.

Keep these stable top-level keys:

```json
{
  "schemaVersion": 2,
  "kind": "code-review",
  "generatedAt": "RFC 3339 timestamp",
  "verdict": "APPROVE | APPROVE_WITH_IMPROVEMENTS | REQUEST_CHANGES",
  "scope": {
    "claim": "string",
    "base": "string",
    "head": "string",
    "dirtyState": "string"
  },
  "coverage": {
    "activeAxes": ["deduplication | claim-and-specification | engineering-quality | discoverability"],
    "skippedAxes": [{"name": "string", "reason": "string"}],
    "staticOnly": ["string"],
    "unreached": ["string"]
  },
  "churn": {
    "window": "string",
    "symbolCoverage": ["string"],
    "fileFallbacks": ["string"]
  },
  "probes": [{"commandOrAction": "string", "result": "string"}],
  "findings": [],
  "verifiedClaims": ["string"],
  "limitations": ["string"]
}
```

Each finding contains:

```json
{
  "id": "CR-N",
  "severity": "BLOCKING | IMPORTANT | IMPROVEMENT | NITPICK",
  "category": "CLAIM | QUALITY | ARCHITECTURE_RISK | DISCOVERABILITY",
  "title": "string",
  "location": {"path": "string", "line": 1, "symbol": "string or null"},
  "evidence": ["string"],
  "impact": "string",
  "remedy": "string",
  "status": "open | fixed | deferred",
  "staticOnly": false,
  "churn": null
}
```

For `ARCHITECTURE_RISK`, replace `churn: null` with:

```json
{
  "granularity": "symbol | file",
  "window": "string",
  "touches": 0,
  "linesAdded": 0,
  "linesDeleted": 0,
  "coSignals": ["string"]
}
```

Use JSON numbers for counts, `null` only where shown, repository-relative paths,
and deterministic finding order by `BLOCKING`, `IMPORTANT`, `IMPROVEMENT`,
`NITPICK`, then ID. Include every reported finding and preserve an empty
`findings` array when none qualify. After writing, parse the file once and
report its path; do not modify other files.
