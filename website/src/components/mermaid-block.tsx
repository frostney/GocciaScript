"use client";

import { useEffect, useState } from "react";

type State =
  | { kind: "loading" }
  | { kind: "rendered"; svg: string }
  | { kind: "error"; message: string; stack?: string };

let renderSeq = 0;

/** Read the current document theme. Server-rendered or pre-mount → `light`. */
function readTheme(): "light" | "dark" {
  if (typeof document === "undefined") return "light";
  return document.documentElement.dataset.theme === "espresso"
    ? "dark"
    : "light";
}

/**
 * Render a Mermaid diagram. Uses `mermaid.run()` against an off-screen DOM
 * node and captures the resulting SVG into state — avoids the
 * `mermaid.render(id, text)` path that's been throwing
 * `Cannot read properties of undefined (reading 'startsWith')` on
 * v11 + Next.js (a known compatibility issue with how `render()` looks up
 * its own injected `<svg>`/`<g>` containers under aggressive code-
 * splitting). `run()` does diagram-type detection, lazy-loads the right
 * parser, and inlines the SVG in one shot.
 *
 * Mermaid is dynamically imported so its ~1MB payload only ships in the
 * chunks for pages that actually have a `mermaid` fence.
 */
export function MermaidBlock({ code }: { code: string }) {
  const [state, setState] = useState<State>({ kind: "loading" });
  const [theme, setTheme] = useState<"light" | "dark">(() => readTheme());

  // Re-render when the user toggles the site theme. We watch
  // `data-theme` on `<html>` rather than depending on a context to keep the
  // component decoupled from any provider.
  useEffect(() => {
    if (typeof document === "undefined") return;
    const obs = new MutationObserver(() => {
      const next = readTheme();
      setTheme((prev) => (prev === next ? prev : next));
    });
    obs.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-theme"],
    });
    return () => obs.disconnect();
  }, []);

  useEffect(() => {
    let cancelled = false;
    (async () => {
      try {
        const m = await import("mermaid");
        const mermaid = m.default;
        mermaid.initialize({
          startOnLoad: false,
          theme: theme === "dark" ? "dark" : "neutral",
          // `strict` disables click handlers and HTML interpolation in
          // diagram source. Our diagrams come from our own docs, but XSS
          // posture is "deny by default" — we don't need either feature, so
          // there's no reason to grant them.
          securityLevel: "strict",
          fontFamily: "ui-monospace, monospace",
        });

        const text = String(code ?? "").trim();
        if (!text) {
          if (!cancelled) setState({ kind: "error", message: "empty diagram" });
          return;
        }

        // Temporary off-screen container — mermaid needs the node to be
        // in the document for d3's selection lookups, but we don't want
        // it visible while it renders.
        const host = document.createElement("div");
        host.style.position = "absolute";
        host.style.left = "-99999px";
        host.style.top = "0";
        host.style.visibility = "hidden";
        document.body.appendChild(host);

        try {
          renderSeq += 1;
          const node = document.createElement("pre");
          node.className = "mermaid";
          node.id = `mermaid-${Date.now().toString(36)}-${renderSeq.toString(36)}`;
          node.textContent = text;
          host.appendChild(node);

          await mermaid.run({ nodes: [node], suppressErrors: false });
          if (!cancelled) setState({ kind: "rendered", svg: node.innerHTML });
        } finally {
          host.remove();
        }
      } catch (err) {
        if (!cancelled) {
          setState({
            kind: "error",
            message: err instanceof Error ? err.message : String(err),
            stack: err instanceof Error ? err.stack : undefined,
          });
        }
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [code, theme]);

  if (state.kind === "error") {
    return (
      <div className="mermaid-block mermaid-block-error">
        <div className="mermaid-block-head">mermaid · render error</div>
        <pre>{state.message}</pre>
        {state.stack && (
          <pre className="mermaid-block-stack">{state.stack}</pre>
        )}
        <pre>{code}</pre>
      </div>
    );
  }
  if (state.kind === "rendered") {
    return (
      <div
        className="mermaid-block mermaid-block-rendered"
        // biome-ignore lint/security/noDangerouslySetInnerHtml: mermaid emits inline SVG sourced from our own docs/
        dangerouslySetInnerHTML={{ __html: state.svg }}
      />
    );
  }
  return (
    <div className="mermaid-block mermaid-block-loading">
      <pre>{code}</pre>
    </div>
  );
}
