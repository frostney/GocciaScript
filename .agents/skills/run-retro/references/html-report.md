# Retrospective HTML impact report

Produce one readable HTML artifact for every retrospective. Use an existing
repository retrospective-output convention when present; otherwise write
`.agent/retrospectives/<yyyy-mm-dd>-<workstream-slug>.html`.

The report is an authorized output of `/run-retro`, independent of later
documentation or ticket selections. It does not authorize committing, pushing,
or publishing the artifact.

## Impact model

Create one card per key delivered change or material workstream outcome. Each
card requires:

- a short title;
- an impact-focused summary of at most 300 Unicode characters;
- an evidence-backed **Before** state;
- an evidence-backed **After** state; and
- a deep-dive section with evidence, uncertainty, a copyable discussion prompt,
  and an optional host-supplied chat URL.

Before and After may be plain text, code, or an image such as a screenshot. Do
not manufacture a visual. Caption images and explain what observable change they
show. Use a text state when no trustworthy code or image artifact exists.

Keep the report scannable. Put timings, provenance, confidence limits, and
longer evidence in deep-dive sections rather than stretching the impact summary.
If the retro finds no durable process lesson, still report the delivered change
and state that no generalized follow-up was supported.

## Renderer input

Create a temporary UTF-8 JSON file with this shape:

```json
{
  "title": "Parser release retrospective",
  "subtitle": "Impact summary and evidence",
  "changes": [
    {
      "title": "Escaped delimiters remain intact",
      "impact": "Users can now parse escaped delimiters without silent token loss.",
      "before": {
        "kind": "code",
        "content": "parse('a\\\\|b') // token was truncated",
        "language": "typescript",
        "caption": "Previous observed behavior"
      },
      "after": {
        "kind": "text",
        "content": "The parser returns the complete escaped token.",
        "caption": "Verified release behavior"
      },
      "deepDive": "Review the regression fixture, review finding, and timing evidence.",
      "discussionPrompt": "Deep-dive this parser impact with me."
    }
  ]
}
```

State `kind` is `text`, `code`, or `image`. For an image, put its URL, data URI,
or local path in `content` and provide `alt`; local images are embedded into the
HTML. `caption`, `language`, `deepDive`, `discussionPrompt`, `deepDiveUrl`, and
`subtitle` are optional. Never invent an app URL. Omit it when the host does not
provide a documented route; the copy control is the fallback.

Cards provide accessible Before, After, and Both views plus an expandable deep
dive. When a delivered change introduces or alters a pattern, lifecycle, state
flow, or interaction across at least three meaningful elements, add a
declarative `diagram` with `title`, at least three `{id, label}` nodes, `{id,
from, to, label?}` edges, and ordered `{label, highlights}` steps. The renderer
creates self-contained SVG with play, pause, step, and reset controls.

Render with:

```bash
python3 <run-retro-skill-directory>/scripts/render_retro.py \
  --input <temporary-json> \
  --output <report.html>
```

The renderer rejects missing states, impact summaries over 300 characters,
unsafe deep-dive URL schemes, and invalid diagram references. Open or render the
result and verify headings, state views, image/code display, deep-dive actions,
diagram controls when present, and the output path before sharing it.
