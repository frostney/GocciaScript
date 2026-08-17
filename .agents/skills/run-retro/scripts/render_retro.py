#!/usr/bin/env python3
"""Render a validated retrospective impact report from JSON."""

from __future__ import annotations

import argparse
import base64
import html
import json
import mimetypes
from pathlib import Path
from typing import Any
from urllib.parse import urlparse


def _required_text(value: Any, field: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{field} must be a non-empty string")
    return value.strip()


def _optional_text(value: Any, field: str) -> str | None:
    if value is None:
        return None
    if not isinstance(value, str):
        raise ValueError(f"{field} must be a string")
    return value.strip() or None


def _image_source(source: str, input_directory: Path) -> str:
    if source.startswith(("data:", "https://", "http://")):
        return source

    image_path = Path(source).expanduser()
    if not image_path.is_absolute():
        image_path = input_directory / image_path
    if not image_path.is_file():
        raise ValueError(f"image does not exist: {source}")

    mime_type = mimetypes.guess_type(image_path.name)[0] or "application/octet-stream"
    payload = base64.b64encode(image_path.read_bytes()).decode("ascii")
    return f"data:{mime_type};base64,{payload}"


def _render_state(state: Any, field: str, input_directory: Path) -> str:
    if not isinstance(state, dict):
        raise ValueError(f"{field} must be an object")

    kind = _required_text(state.get("kind"), f"{field}.kind")
    content = _required_text(state.get("content"), f"{field}.content")
    caption = _optional_text(state.get("caption"), f"{field}.caption")

    if kind == "text":
        body = f"<p>{html.escape(content)}</p>"
    elif kind == "code":
        language = _optional_text(state.get("language"), f"{field}.language") or "text"
        body = (
            f'<pre><code class="language-{html.escape(language)}">'
            f"{html.escape(content)}</code></pre>"
        )
    elif kind == "image":
        alt = _required_text(state.get("alt"), f"{field}.alt")
        source = _image_source(content, input_directory)
        body = f'<img src="{html.escape(source)}" alt="{html.escape(alt)}">'
    else:
        raise ValueError(f"{field}.kind must be text, code, or image")

    if caption:
        body += f"<p class=\"caption\">{html.escape(caption)}</p>"
    return body


def _deep_dive_url(value: Any, field: str) -> str | None:
    url = _optional_text(value, field)
    if url is None:
        return None
    parsed = urlparse(url)
    if parsed.scheme not in {"codex", "https", "http"}:
        raise ValueError(f"{field} must use codex, https, or http")
    return url


def _render_diagram(diagram: Any, field: str, diagram_index: int) -> str:
    if diagram is None:
        return ""
    if not isinstance(diagram, dict):
        raise ValueError(f"{field} must be an object")

    title = _required_text(diagram.get("title"), f"{field}.title")
    nodes = diagram.get("nodes")
    edges = diagram.get("edges")
    steps = diagram.get("steps")
    if not isinstance(nodes, list) or len(nodes) < 3:
        raise ValueError(f"{field}.nodes must contain at least three items")
    if not isinstance(edges, list):
        raise ValueError(f"{field}.edges must be an array")
    if not isinstance(steps, list) or not steps:
        raise ValueError(f"{field}.steps must contain at least one item")

    node_ids: set[str] = set()
    rendered_nodes: list[str] = []
    positions: dict[str, tuple[int, int]] = {}
    columns = min(3, len(nodes))
    rows = (len(nodes) + columns - 1) // columns
    height = 90 + rows * 120
    for node_index, node in enumerate(nodes):
        if not isinstance(node, dict):
            raise ValueError(f"{field}.nodes[{node_index}] must be an object")
        node_id = _required_text(node.get("id"), f"{field}.nodes[{node_index}].id")
        label = _required_text(node.get("label"), f"{field}.nodes[{node_index}].label")
        if node_id in node_ids:
            raise ValueError(f"{field}.nodes contains duplicate id {node_id}")
        node_ids.add(node_id)
        column = node_index % columns
        row = node_index // columns
        x = int((column + 0.5) * (720 / columns))
        y = 75 + row * 120
        positions[node_id] = (x, y)
        rendered_nodes.append(
            f'<g class="diagram-node" data-element-id="{html.escape(node_id, quote=True)}">'
            f'<rect x="{x - 88}" y="{y - 28}" width="176" height="56" rx="12" />'
            f'<text x="{x}" y="{y + 5}">{html.escape(label)}</text></g>'
        )

    element_ids = set(node_ids)
    rendered_edges: list[str] = []
    marker_id = f"arrow-{diagram_index}"
    for edge_index, edge in enumerate(edges):
        if not isinstance(edge, dict):
            raise ValueError(f"{field}.edges[{edge_index}] must be an object")
        edge_id = _required_text(edge.get("id"), f"{field}.edges[{edge_index}].id")
        source = _required_text(edge.get("from"), f"{field}.edges[{edge_index}].from")
        target = _required_text(edge.get("to"), f"{field}.edges[{edge_index}].to")
        label = _optional_text(edge.get("label"), f"{field}.edges[{edge_index}].label")
        if edge_id in element_ids:
            raise ValueError(f"{field}.edges contains duplicate id {edge_id}")
        if source not in positions or target not in positions:
            raise ValueError(f"{field}.edges[{edge_index}] references an unknown node")
        element_ids.add(edge_id)
        x1, y1 = positions[source]
        x2, y2 = positions[target]
        label_svg = ""
        if label:
            label_svg = (
                f'<text x="{(x1 + x2) // 2}" y="{(y1 + y2) // 2 - 8}">'
                f"{html.escape(label)}</text>"
            )
        rendered_edges.append(
            f'<g class="diagram-edge" data-element-id="{html.escape(edge_id, quote=True)}">'
            f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" '
            f'marker-end="url(#{marker_id})" />{label_svg}</g>'
        )

    normalized_steps: list[dict[str, Any]] = []
    for step_index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise ValueError(f"{field}.steps[{step_index}] must be an object")
        label = _required_text(step.get("label"), f"{field}.steps[{step_index}].label")
        highlights = step.get("highlights")
        if not isinstance(highlights, list) or not highlights:
            raise ValueError(
                f"{field}.steps[{step_index}].highlights must contain at least one id"
            )
        if not all(isinstance(item, str) and item in element_ids for item in highlights):
            raise ValueError(f"{field}.steps[{step_index}] references an unknown id")
        normalized_steps.append({"label": label, "highlights": highlights})

    step_data = html.escape(json.dumps(normalized_steps), quote=True)
    return f"""
        <section class="mechanism-diagram" data-diagram data-steps="{step_data}">
          <h3>{html.escape(title)}</h3>
          <svg viewBox="0 0 720 {height}" role="img" aria-label="{html.escape(title, quote=True)}">
            <defs><marker id="{marker_id}" markerWidth="10" markerHeight="7" refX="9" refY="3.5" orient="auto"><polygon points="0 0, 10 3.5, 0 7" /></marker></defs>
            {''.join(rendered_edges)}
            {''.join(rendered_nodes)}
          </svg>
          <div class="diagram-controls" aria-label="Diagram playback controls">
            <button type="button" data-diagram-action="play">Play</button>
            <button type="button" data-diagram-action="pause">Pause</button>
            <button type="button" data-diagram-action="step">Step</button>
            <button type="button" data-diagram-action="reset">Reset</button>
            <span data-diagram-status role="status">Ready</span>
          </div>
        </section>"""


def render_report(data: Any, input_directory: Path) -> str:
    if not isinstance(data, dict):
        raise ValueError("report must be an object")

    title = _required_text(data.get("title"), "title")
    subtitle = _optional_text(data.get("subtitle"), "subtitle")
    changes = data.get("changes")
    if not isinstance(changes, list) or not changes:
        raise ValueError("changes must contain at least one item")

    cards: list[str] = []
    for index, change in enumerate(changes, start=1):
        if not isinstance(change, dict):
            raise ValueError(f"changes[{index}] must be an object")
        change_title = _required_text(change.get("title"), f"changes[{index}].title")
        impact = _required_text(change.get("impact"), f"changes[{index}].impact")
        if len(impact) > 300:
            raise ValueError(
                f"changes[{index}].impact exceeds 300 characters ({len(impact)})"
            )
        before = _render_state(
            change.get("before"), f"changes[{index}].before", input_directory
        )
        after = _render_state(
            change.get("after"), f"changes[{index}].after", input_directory
        )
        deep_dive = _optional_text(
            change.get("deepDive"), f"changes[{index}].deepDive"
        ) or "No additional evidence was recorded."
        discussion_prompt = _optional_text(
            change.get("discussionPrompt"), f"changes[{index}].discussionPrompt"
        ) or f'Deep-dive "{change_title}" with me.'
        deep_dive_url = _deep_dive_url(
            change.get("deepDiveUrl"), f"changes[{index}].deepDiveUrl"
        )
        diagram = _render_diagram(
            change.get("diagram"), f"changes[{index}].diagram", index
        )
        deep_dive_link = ""
        if deep_dive_url:
            deep_dive_link = (
                f'<a class="button-link" href="{html.escape(deep_dive_url, quote=True)}">'
                "Open deep-dive chat</a>"
            )

        cards.append(
            f"""
      <article class="change-card" id="change-{index}">
        <div class="change-number">Change {index}</div>
        <h2>{html.escape(change_title)}</h2>
        <p class="impact">{html.escape(impact)}</p>
        <div class="state-controls" aria-label="Before and After view">
          <button type="button" data-state-view="before">Before</button>
          <button type="button" data-state-view="after">After</button>
          <button type="button" data-state-view="both" aria-pressed="true">Both</button>
        </div>
        <div class="states" data-states>
          <section data-state="before"><h3>Before</h3>{before}</section>
          <section data-state="after"><h3>After</h3>{after}</section>
        </div>
        {diagram}
        <details>
          <summary>Deep dive</summary>
          <p>{html.escape(deep_dive)}</p>
          <p class="prompt"><strong>Continue with the user:</strong> {html.escape(discussion_prompt)}</p>
          <div class="deep-actions">
            {deep_dive_link}
            <button type="button" data-copy-prompt="{html.escape(discussion_prompt, quote=True)}">Copy deep-dive prompt</button>
            <span data-copy-status role="status"></span>
          </div>
        </details>
      </article>"""
        )

    subtitle_html = f'<p class="subtitle">{html.escape(subtitle)}</p>' if subtitle else ""
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{html.escape(title)}</title>
  <style>
    :root {{ color-scheme: light dark; font-family: Inter, ui-sans-serif, system-ui, sans-serif; }}
    body {{ margin: 0; background: #f3f4f6; color: #172033; }}
    main {{ width: min(1120px, calc(100% - 32px)); margin: 48px auto; }}
    header {{ margin-bottom: 28px; }}
    h1 {{ margin: 0; font-size: clamp(2rem, 5vw, 4rem); letter-spacing: -0.04em; }}
    .subtitle {{ color: #526078; font-size: 1.1rem; }}
    .change-card {{ background: white; border: 1px solid #d9deea; border-radius: 18px; box-shadow: 0 12px 36px rgb(41 53 78 / 10%); margin: 24px 0; padding: 28px; }}
    .change-number {{ color: #3659d9; font-size: .78rem; font-weight: 800; letter-spacing: .12em; text-transform: uppercase; }}
    h2 {{ margin: 8px 0; }}
    .impact {{ max-width: 76ch; font-size: 1.08rem; line-height: 1.6; }}
    button, .button-link {{ background: #eef2ff; border: 1px solid #c7d2fe; border-radius: 8px; color: #263b8f; cursor: pointer; display: inline-block; font: inherit; padding: 8px 12px; text-decoration: none; }}
    button[aria-pressed="true"] {{ background: #3659d9; color: white; }}
    .state-controls, .deep-actions, .diagram-controls {{ align-items: center; display: flex; flex-wrap: wrap; gap: 8px; }}
    .states {{ display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 18px; margin: 16px 0 24px; }}
    .states section {{ background: #f7f8fb; border-radius: 12px; min-width: 0; padding: 18px; }}
    .states h3 {{ margin-top: 0; }}
    pre {{ overflow: auto; background: #172033; color: #f7f8fb; border-radius: 9px; padding: 14px; }}
    img {{ display: block; max-width: 100%; border-radius: 9px; }}
    .caption {{ color: #647089; font-size: .86rem; }}
    details {{ border-top: 1px solid #d9deea; padding-top: 18px; }}
    summary {{ cursor: pointer; font-weight: 750; }}
    .prompt {{ background: #eef2ff; border-radius: 9px; padding: 12px; }}
    .mechanism-diagram {{ margin: 24px 0; }}
    .mechanism-diagram svg {{ background: #f7f8fb; border-radius: 12px; width: 100%; }}
    .diagram-node rect {{ fill: white; stroke: #7c8bb1; stroke-width: 2; transition: fill .2s, stroke .2s; }}
    .diagram-node text, .diagram-edge text {{ fill: currentColor; font-size: 14px; text-anchor: middle; }}
    .diagram-edge line {{ stroke: #7c8bb1; stroke-width: 2; }}
    .diagram-edge polygon {{ fill: #7c8bb1; }}
    .diagram-node.is-active rect {{ fill: #dbe4ff; stroke: #3659d9; stroke-width: 4; }}
    .diagram-edge.is-active line {{ stroke: #3659d9; stroke-width: 5; }}
    .diagram-controls {{ margin-top: 10px; }}
    @media (max-width: 720px) {{ .states {{ grid-template-columns: 1fr; }} main {{ margin-top: 28px; }} }}
    @media (prefers-color-scheme: dark) {{
      body {{ background: #0d1320; color: #eef2ff; }}
      .subtitle, .caption {{ color: #aab6cf; }}
      .change-card {{ background: #151d2d; border-color: #2c3850; }}
      .states section {{ background: #101827; }}
      .prompt {{ background: #202c4b; }}
      button, .button-link {{ background: #202c4b; border-color: #43537a; color: #dbe4ff; }}
      .mechanism-diagram svg {{ background: #101827; }}
      .diagram-node rect {{ fill: #151d2d; }}
      .diagram-node.is-active rect {{ fill: #263b8f; }}
      details {{ border-color: #2c3850; }}
    }}
    @media (prefers-reduced-motion: reduce) {{ * {{ scroll-behavior: auto !important; transition: none !important; }} }}
  </style>
</head>
<body>
  <main>
    <header><h1>{html.escape(title)}</h1>{subtitle_html}</header>
    {''.join(cards)}
  </main>
  <script>
    document.querySelectorAll('.change-card').forEach((card) => {{
      card.querySelectorAll('[data-state-view]').forEach((button) => {{
        button.addEventListener('click', () => {{
          const view = button.dataset.stateView;
          card.querySelectorAll('[data-state-view]').forEach((candidate) => {{
            candidate.setAttribute('aria-pressed', String(candidate === button));
          }});
          card.querySelectorAll('[data-state]').forEach((state) => {{
            state.hidden = view !== 'both' && state.dataset.state !== view;
          }});
        }});
      }});
      const copy = card.querySelector('[data-copy-prompt]');
      if (copy) copy.addEventListener('click', async () => {{
        const prompt = copy.dataset.copyPrompt;
        const status = card.querySelector('[data-copy-status]');
        try {{
          if (!navigator.clipboard) throw new Error('Clipboard unavailable');
          await navigator.clipboard.writeText(prompt);
        }} catch (_) {{
          const area = document.createElement('textarea');
          area.value = prompt;
          document.body.appendChild(area);
          area.select();
          document.execCommand('copy');
          area.remove();
        }}
        status.textContent = 'Copied';
      }});
    }});

    document.querySelectorAll('[data-diagram]').forEach((diagram) => {{
      const steps = JSON.parse(diagram.dataset.steps);
      const status = diagram.querySelector('[data-diagram-status]');
      let current = -1;
      let timer = null;
      const show = (index) => {{
        current = index;
        const step = current >= 0 ? steps[current] : null;
        diagram.querySelectorAll('[data-element-id]').forEach((element) => {{
          element.classList.toggle('is-active', Boolean(step && step.highlights.includes(element.dataset.elementId)));
        }});
        status.textContent = step ? `${{current + 1}}/${{steps.length}}: ${{step.label}}` : 'Ready';
      }};
      const pause = () => {{ if (timer) clearInterval(timer); timer = null; }};
      diagram.querySelector('[data-diagram-action="play"]').addEventListener('click', () => {{
        pause();
        show((current + 1) % steps.length);
        timer = setInterval(() => show((current + 1) % steps.length), 1200);
      }});
      diagram.querySelector('[data-diagram-action="pause"]').addEventListener('click', pause);
      diagram.querySelector('[data-diagram-action="step"]').addEventListener('click', () => {{ pause(); show((current + 1) % steps.length); }});
      diagram.querySelector('[data-diagram-action="reset"]').addEventListener('click', () => {{ pause(); show(-1); }});
    }});
  </script>
</body>
</html>
"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    arguments = parser.parse_args()

    input_path = arguments.input.resolve()
    data = json.loads(input_path.read_text(encoding="utf-8"))
    rendered = render_report(data, input_path.parent)
    output_path = arguments.output.resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(rendered, encoding="utf-8")
    print(output_path)


if __name__ == "__main__":
    main()
