"use client";

import Link from "next/link";
import { Fragment, type ReactNode, useMemo } from "react";
import { slugify } from "@/components/anchor-heading";
import {
  HighlightedCode,
  HighlightedGeneric,
  HighlightedJson,
  HighlightedShell,
} from "@/components/highlighted-code";
import { MermaidBlock } from "@/components/mermaid-block";
import { DOC_HREF_MAP } from "@/lib/docs-data";

/** Map markdown fence info-strings to one of the highlighter dispatchers. */
function renderHighlighted(code: string, lang: string): ReactNode {
  const l = lang.toLowerCase().trim();
  if (l === "javascript" || l === "js" || l === "jsx") {
    return <HighlightedCode code={code} />;
  }
  if (l === "typescript" || l === "ts" || l === "tsx") {
    return <HighlightedGeneric code={code} language="ts" />;
  }
  if (l === "pascal" || l === "pas" || l === "fpc" || l === "delphi") {
    return <HighlightedGeneric code={code} language="pascal" />;
  }
  if (l === "csharp" || l === "cs" || l === "c#") {
    return <HighlightedGeneric code={code} language="csharp" />;
  }
  if (l === "cpp" || l === "c++" || l === "cxx") {
    return <HighlightedGeneric code={code} language="cpp" />;
  }
  if (l === "crystal" || l === "cr") {
    return <HighlightedGeneric code={code} language="crystal" />;
  }
  if (l === "json") {
    return <HighlightedJson code={code} />;
  }
  if (l === "shell" || l === "sh" || l === "bash" || l === "zsh") {
    return <HighlightedShell code={code} />;
  }
  return code;
}

type InlineToken =
  | { kind: "text"; text: string }
  | { kind: "code"; text: string }
  | { kind: "strong"; text: string }
  | { kind: "em"; text: string }
  | { kind: "image"; alt: string; src: string }
  | { kind: "link"; text: string; href: string };

const PATTERNS: { re: RegExp; build: (m: RegExpMatchArray) => InlineToken }[] =
  [
    { re: /`([^`]+)`/, build: (m) => ({ kind: "code", text: m[1] }) },
    { re: /\*\*([^*]+)\*\*/, build: (m) => ({ kind: "strong", text: m[1] }) },
    { re: /\*([^*]+)\*/, build: (m) => ({ kind: "em", text: m[1] }) },
    {
      re: /!\[([^\]]*)\]\(([^)]+)\)/,
      build: (m) => ({ kind: "image", alt: m[1], src: m[2] }),
    },
    {
      re: /\[([^\]]+)\]\(([^)]+)\)/,
      build: (m) => ({ kind: "link", text: m[1], href: m[2] }),
    },
  ];

/** Schemes we allow in fallback `<a href>` rendering. Anything else
 *  (e.g. `javascript:`, `data:`, `vbscript:`) falls through to plain
 *  text — defense in depth even though our docs corpus is trusted. */
const SAFE_HREF_SCHEMES = new Set(["http", "https", "mailto", "tel"]);

/** Return `href` if it's safe to use directly, otherwise `null`.
 *  Safe forms: in-page anchor (`#…`), root-relative path (`/…`),
 *  protocol-relative (`//…`), or one of the allowlisted schemes.
 *
 *  Browsers strip TAB / CR / LF / leading whitespace from `href` values
 *  before navigating, so a naïve scheme regex on the raw input would let
 *  `" javascript:"` or `"j\tavascript:"` slip through and fire the
 *  dangerous scheme. We reject anything containing control characters
 *  outright, then trim and validate. */
export function safeHref(href: string): string | null {
  if (!href) return null;
  // C0 control range (0x00–0x1F) + DEL (0x7F). If browsers would silently
  // strip these to reach a different scheme, treat the href as unsafe
  // rather than try to mirror the stripping algorithm exactly.
  for (let i = 0; i < href.length; i++) {
    const code = href.charCodeAt(i);
    // C0 controls (0x00-0x1F) plus DEL (0x7F).
    if (code < 0x20 || code === 0x7f) return null;
  }
  const h = href.trim();
  if (!h) return null;
  if (h.startsWith("#") || h.startsWith("/")) return h;
  const m = h.match(/^([a-z][a-z0-9+.-]*):/i);
  if (!m) return h; // bare relative path or fragment-relative
  return SAFE_HREF_SCHEMES.has(m[1].toLowerCase()) ? h : null;
}

/** True when `line` is a markdown horizontal rule (`---`, `------`, etc.). */
function isHr(line: string): boolean {
  return /^---+\s*$/.test(line);
}

/** True when `line` + `next` together form a markdown table head + delimiter
 *  (a `|`-bearing header line followed by a `---`-bearing delimiter line). */
function isTableStart(line: string, next: string): boolean {
  return /\|/.test(line) && /\|/.test(next) && /-{3,}/.test(next);
}

function tokenizeInline(text: string): InlineToken[] {
  const out: InlineToken[] = [];
  let rest = text;
  while (rest.length) {
    let earliest: {
      idx: number;
      m: RegExpMatchArray;
      build: (m: RegExpMatchArray) => InlineToken;
    } | null = null;
    for (const p of PATTERNS) {
      const m = rest.match(p.re);
      if (
        m &&
        m.index !== undefined &&
        (earliest === null || m.index < earliest.idx)
      ) {
        earliest = { idx: m.index, m, build: p.build };
      }
    }
    if (!earliest) {
      out.push({ kind: "text", text: rest });
      break;
    }
    if (earliest.idx > 0) {
      out.push({ kind: "text", text: rest.slice(0, earliest.idx) });
    }
    out.push(earliest.build(earliest.m));
    rest = rest.slice(earliest.idx + earliest.m[0].length);
  }
  return out;
}

/** Resolve a markdown link to an internal `/docs/...` path. Returns null
 *  when the link is external or doesn't map to any known doc. */
function resolveDocHref(href: string): string | null {
  // Skip protocol-prefixed (https://, mailto:, etc.) and pure anchors.
  if (/^[a-z][a-z0-9+.-]*:/i.test(href)) return null;
  if (href.startsWith("#")) return null;

  // Split off any anchor fragment so it carries through to the resolved URL.
  const hashIdx = href.indexOf("#");
  const pathPart = hashIdx >= 0 ? href.slice(0, hashIdx) : href;
  const hash = hashIdx >= 0 ? href.slice(hashIdx) : "";

  // Normalize path:
  //   ../docs/architecture.md → architecture
  //   docs/architecture.md    → architecture
  //   architecture.md         → architecture
  //   contributing/workflow.md → contributing/workflow
  const normalized = pathPart
    .replace(/^\.\.\//, "")
    .replace(/^\.\//, "")
    .replace(/^docs\//, "")
    .replace(/\.md$/i, "");

  if (!normalized) return null;
  const id = DOC_HREF_MAP[normalized];
  return id ? `/docs/${id}${hash}` : null;
}

function renderInline(text: string): ReactNode[] {
  return tokenizeInline(text).map((tok, i) => {
    switch (tok.kind) {
      case "text":
        return <Fragment key={i}>{tok.text}</Fragment>;
      case "code":
        return <code key={i}>{tok.text}</code>;
      case "strong":
        return <strong key={i}>{tok.text}</strong>;
      case "em":
        return <em key={i}>{tok.text}</em>;
      case "image": {
        let src = tok.src;
        if (src === "./logo.png") src = "/logo.png";
        return (
          // biome-ignore lint/performance/noImgElement: markdown body inserts dynamic images, no fixed dimensions known
          <img
            key={i}
            src={src}
            alt={tok.alt}
            className="block my-2 max-w-[180px]"
          />
        );
      }
      case "link": {
        // Pure in-page anchors stay in-page.
        if (tok.href.startsWith("#")) {
          return (
            <a key={i} href={tok.href}>
              {tok.text}
            </a>
          );
        }
        const docHref = resolveDocHref(tok.href);
        if (docHref) {
          return (
            <Link key={i} href={docHref}>
              {tok.text}
            </Link>
          );
        }
        // External / unknown link. Sanitize the scheme — `javascript:`
        // and friends fall through to plain text rather than becoming a
        // navigable element. Markdown body comes from our own docs, so
        // this is defense in depth, not the primary defense.
        const safe = safeHref(tok.href);
        if (!safe) {
          return <Fragment key={i}>{tok.text}</Fragment>;
        }
        return (
          <a key={i} href={safe} target="_blank" rel="noopener noreferrer">
            {tok.text}
          </a>
        );
      }
      default:
        return null;
    }
  });
}

export type Block =
  | { kind: "heading"; level: number; text: string }
  | { kind: "hr" }
  | { kind: "quote"; text: string }
  | { kind: "list"; items: string[] }
  | { kind: "table"; hdr: string[]; rows: string[][] }
  | { kind: "code"; lang: string; code: string }
  | { kind: "p"; text: string };

export function parseMarkdown(source: string): Block[] {
  const lines = source.replace(/\r\n/g, "\n").split("\n");
  const blocks: Block[] = [];
  let i = 0;
  while (i < lines.length) {
    const line = lines[i];

    if (/^```/.test(line)) {
      const lang = line.slice(3).trim();
      const buf: string[] = [];
      i++;
      while (i < lines.length && !/^```/.test(lines[i])) {
        buf.push(lines[i]);
        i++;
      }
      i++;
      blocks.push({ kind: "code", lang, code: buf.join("\n") });
      continue;
    }

    const hm = line.match(/^(#{1,4})\s+(.*)$/);
    if (hm) {
      blocks.push({ kind: "heading", level: hm[1].length, text: hm[2] });
      i++;
      continue;
    }

    if (isHr(line)) {
      blocks.push({ kind: "hr" });
      i++;
      continue;
    }

    if (/^>\s?/.test(line)) {
      const buf: string[] = [];
      while (i < lines.length && /^>\s?/.test(lines[i])) {
        buf.push(lines[i].replace(/^>\s?/, ""));
        i++;
      }
      blocks.push({ kind: "quote", text: buf.join("\n") });
      continue;
    }

    if (isTableStart(line, lines[i + 1] || "")) {
      // Split table cells on `|`, but treat `\|` as a literal pipe.
      // Uses a long, unlikely sentinel rather than a single control char
      // so collisions with arbitrary input remain effectively impossible
      // even for content that happens to use control characters.
      const PIPE_PLACEHOLDER = "__GOCCIA_MD_ESCAPED_PIPE__";
      const PIPE_RESTORE = new RegExp(PIPE_PLACEHOLDER, "g");
      const splitRow = (raw: string) =>
        raw
          .replace(/\\\|/g, PIPE_PLACEHOLDER)
          .split("|")
          .map((c) => c.trim().replace(PIPE_RESTORE, "|"));
      // Strip ONLY the leading/trailing empty cells produced by the
      // outer `|` markers. Using `filter(Boolean)` for the header would
      // also drop intentional empty cells (`| a | | b |`) and
      // desynchronize column counts vs. the rows array.
      const stripOuterEmpty = (cells: string[]) =>
        cells.filter(
          (c, idx, arr) =>
            !(idx === 0 && c === "") && !(idx === arr.length - 1 && c === ""),
        );
      const hdr = stripOuterEmpty(splitRow(line));
      i += 2;
      const rows: string[][] = [];
      while (i < lines.length && /\|/.test(lines[i]) && lines[i].trim()) {
        rows.push(stripOuterEmpty(splitRow(lines[i])));
        i++;
      }
      blocks.push({ kind: "table", hdr, rows });
      continue;
    }

    if (/^[-*]\s+/.test(line)) {
      const items: string[] = [];
      while (i < lines.length && /^[-*]\s+/.test(lines[i])) {
        items.push(lines[i].replace(/^[-*]\s+/, ""));
        i++;
      }
      blocks.push({ kind: "list", items });
      continue;
    }

    if (!line.trim()) {
      i++;
      continue;
    }

    const buf = [line];
    i++;
    while (
      i < lines.length &&
      lines[i].trim() &&
      // Block-starting markers: heading, fence, blockquote, list item.
      !/^(#{1,4}\s|```|>\s?|[-*]\s)/.test(lines[i]) &&
      // A bare `---` line is a horizontal rule, not a paragraph
      // continuation — without this guard, paragraphs swallow HRs.
      !isHr(lines[i]) &&
      // The header row of a `| col | col |` / `| --- | --- |` table is
      // a paragraph-shaped line and would otherwise be slurped.
      !isTableStart(lines[i], lines[i + 1] || "")
    ) {
      buf.push(lines[i]);
      i++;
    }
    blocks.push({ kind: "p", text: buf.join(" ") });
  }
  return blocks;
}

export function Markdown({ source }: { source: string }) {
  const blocks = useMemo(() => parseMarkdown(source), [source]);

  // Counter shared across all headings in this document. Repeated heading
  // text would otherwise produce duplicate `id` attributes — anchors then
  // jump to the first occurrence only. Append `-2`, `-3`, … for each
  // subsequent appearance so every heading id is unique and the anchor
  // href matches the final id.
  const seenSlugs = new Map<string, number>();
  const uniqueSlug = (text: string): string => {
    const base = slugify(text);
    const count = (seenSlugs.get(base) ?? 0) + 1;
    seenSlugs.set(base, count);
    return count === 1 ? base : `${base}-${count}`;
  };

  return (
    <div className="docs-main">
      {blocks.map((b, idx) => {
        switch (b.kind) {
          case "heading": {
            const level = Math.min(b.level, 4);
            const inline = renderInline(b.text);
            // h1 is the document/page title; keep it plain (no anchor link).
            if (level === 1) return <h1 key={idx}>{inline}</h1>;
            const slug = uniqueSlug(b.text);
            const anchor = (
              <a
                href={`#${slug}`}
                className="heading-anchor"
                aria-label="Link to this section"
              >
                #
              </a>
            );
            if (level === 2)
              return (
                <h2 key={idx} id={slug} className="anchor-heading">
                  {inline}
                  {anchor}
                </h2>
              );
            if (level === 3)
              return (
                <h3 key={idx} id={slug} className="anchor-heading">
                  {inline}
                  {anchor}
                </h3>
              );
            return (
              <h4 key={idx} id={slug} className="anchor-heading">
                {inline}
                {anchor}
              </h4>
            );
          }
          case "hr":
            return <hr key={idx} />;
          case "quote":
            return <blockquote key={idx}>{renderInline(b.text)}</blockquote>;
          case "list":
            return (
              <ul key={idx}>
                {b.items.map((it, j) => (
                  <li key={j}>{renderInline(it)}</li>
                ))}
              </ul>
            );
          case "table":
            return (
              <table key={idx}>
                <thead>
                  <tr>
                    {b.hdr.map((h, j) => (
                      <th key={j}>{renderInline(h)}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {b.rows.map((r, j) => (
                    <tr key={j}>
                      {r.map((c, k) => (
                        <td key={k}>{renderInline(c)}</td>
                      ))}
                    </tr>
                  ))}
                </tbody>
              </table>
            );
          case "code":
            if ((b.lang || "").trim().toLowerCase() === "mermaid") {
              return <MermaidBlock key={idx} code={b.code} />;
            }
            return (
              <pre key={idx}>
                <code>{renderHighlighted(b.code, b.lang)}</code>
              </pre>
            );
          case "p":
            return <p key={idx}>{renderInline(b.text)}</p>;
          default:
            return null;
        }
      })}
    </div>
  );
}
