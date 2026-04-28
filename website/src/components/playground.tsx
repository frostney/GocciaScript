"use client";

import { useSearchParams } from "next/navigation";
import { useCallback, useEffect, useId, useRef, useState } from "react";
import { AnimatedOutput } from "@/components/animated-output";
import { ConsolePanel } from "@/components/console-panel";
import { HighlightedCode } from "@/components/highlighted-code";
import {
  CopyIcon,
  RunIcon,
  SparkleIcon,
  TerminalIcon,
} from "@/components/icons";
import { AUTOCOMPLETE_TOKENS, EXAMPLES, type OutputLine } from "@/lib/examples";
import { formatError } from "@/lib/format-error";
import { loadCode, saveCode } from "@/lib/playground-storage";
import { decodeShare, encodeShare } from "@/lib/share";

type Backend = "tree-walk" | "bytecode";

function pickExampleId(requested: string | null): string {
  if (requested && EXAMPLES.some((e) => e.id === requested)) return requested;
  return EXAMPLES[0].id;
}

/** Normalize a release tag (`0.6.1` or `v0.6.1`) to the displayed form `v0.6.1`. */
function normalizeTag(tag: string): string {
  return tag.startsWith("v") ? tag : `v${tag}`;
}

type PlaygroundProps = {
  /** Recent stable release tags from GitHub, newest first. The component
   *  appends `nightly` after these. */
  stableTags?: string[];
};

export function Playground({ stableTags = [] }: PlaygroundProps) {
  const params = useSearchParams();

  // Stable per-render IDs so `<label htmlFor>` / `aria-labelledby` reliably
  // associate the visible label with each control for screen readers.
  const exampleId = useId();
  const backendId = useId();
  const asiId = useId();
  const versionId = useId();
  const editorId = useId();

  // Build the version dropdown from the live tag list, falling back to a
  // single `nightly` entry if the GitHub fetch returned nothing.
  const versions: string[] = [...stableTags.map(normalizeTag), "nightly"];

  const [exId, setExId] = useState(() =>
    pickExampleId(params?.get("example") ?? null),
  );
  const example = EXAMPLES.find((e) => e.id === exId) ?? EXAMPLES[0];

  const [code, setCode] = useState(example.code);
  const [output, setOutput] = useState<OutputLine[]>([]);
  const [running, setRunning] = useState(false);
  const [backend, setBackend] = useState<Backend>("tree-walk");
  const [version, setVersion] = useState<string>(versions[0]);
  const [asi, setAsi] = useState(true);
  const [shareTick, setShareTick] = useState(0);

  // Ghost-text autocomplete: track caret + derive a single suggested
  // completion that's painted faded into the highlight overlay at the
  // caret position. Tab accepts.
  const [caret, setCaret] = useState(0);
  const taRef = useRef<HTMLTextAreaElement>(null);
  const hlRef = useRef<HTMLPreElement>(null);
  const hydratedRef = useRef(false);
  // Skip the example-restore effect's first fire so it doesn't clobber the
  // share/saved code that the mount effect just set.
  const initialMountRef = useRef(true);

  /** Suggest a single completion for the word the caret is currently inside. */
  function ghostFor(value: string, pos: number): string {
    const before = value.slice(0, pos);
    const after = value.slice(pos);
    // Only show ghost when the next character isn't part of an identifier —
    // otherwise we'd be inserting in the middle of a word.
    if (after && /^[A-Za-z0-9_$.]/.test(after)) return "";
    const match = before.match(/([A-Za-z_$][\w$.]*)$/);
    if (!match) return "";
    const prefix = match[1];
    if (prefix.length < 2) return "";
    const token = AUTOCOMPLETE_TOKENS.find(
      (t) => t.toLowerCase().startsWith(prefix.toLowerCase()) && t !== prefix,
    );
    if (!token) return "";
    return token.slice(prefix.length);
  }
  const ghost = ghostFor(code, caret);

  // On mount: prefer ?share=, then localStorage, then the example's default.
  // biome-ignore lint/correctness/useExhaustiveDependencies: mount-only sync
  useEffect(() => {
    const shareParam = params?.get("share");
    if (shareParam) {
      const payload = decodeShare(shareParam);
      if (payload?.code) {
        setCode(payload.code);
        if (payload.mode === "tree-walk" || payload.mode === "bytecode") {
          setBackend(payload.mode);
        }
        if (typeof payload.asi === "boolean") setAsi(payload.asi);
        if (payload.version && versions.includes(payload.version)) {
          setVersion(payload.version);
        }
        hydratedRef.current = true;
        return;
      }
    }
    const saved = loadCode(exId);
    setCode(saved ?? example.code);
    hydratedRef.current = true;
  }, []);

  // Switch examples: clear output, no auto-run, restore saved code if present.
  // Skip the first run on mount — the share/saved load above has already
  // populated `code`, and re-running this would overwrite it with the
  // current example's default.
  useEffect(() => {
    if (initialMountRef.current) {
      initialMountRef.current = false;
      return;
    }
    const saved = loadCode(exId);
    setCode(saved ?? example.code);
    setOutput([]);
  }, [exId, example.code]);

  // Persist edits per example.
  useEffect(() => {
    if (!hydratedRef.current) return;
    saveCode(exId, code);
  }, [exId, code]);

  const run = useCallback(async () => {
    setRunning(true);
    const banner = `GocciaScriptLoader --mode=${
      backend === "bytecode" ? "bytecode" : "interpreted"
    }${asi ? " --asi" : ""} ${version}`;
    setOutput([{ kind: "meta", text: banner }]);

    try {
      const res = await fetch("/api/execute", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ code, mode: backend, asi }),
      });
      if (res.status === 429) {
        const retry = res.headers.get("Retry-After") ?? "60";
        setOutput([
          { kind: "meta", text: banner },
          {
            kind: "err",
            text: `Rate limit exceeded — try again in ${retry}s.`,
          },
        ]);
        return;
      }
      // Two error shapes are possible:
      //   • Transport errors (rate limit, bad body, oversize, spawn failed)
      //     short-circuit with `error: { message, code }` and no other fields.
      //   • Successful runner invocations attach the runner's own structured
      //     error (different schema) inside the same `error` slot.
      // Disambiguate by checking for runner-only fields.
      const data = (await res.json()) as {
        ok?: boolean;
        output?: string;
        value?: unknown;
        error?: {
          type?: string;
          name?: string;
          message: string;
          line?: number | null;
          column?: number | null;
          fileName?: string | null;
          code?: string;
        } | null;
        timing?: { total_ms: number };
        exitCode?: number | null;
        truncated?: boolean;
        stderr?: string;
        rawStdout?: string;
      };

      const lines: OutputLine[] = [{ kind: "meta", text: banner }];
      if (data.output) {
        for (const line of data.output.replace(/\n$/, "").split("\n")) {
          lines.push({ kind: "log", text: line });
        }
      }
      if (data.error) {
        for (const line of formatError(data.error, code)) {
          lines.push(line);
        }
      }
      if (data.value !== null && data.value !== undefined) {
        lines.push({
          kind: "result",
          text: `↳ ${JSON.stringify(data.value)}`,
        });
      }
      if (data.truncated) {
        lines.push({
          kind: "meta",
          text: "⚠ output truncated (limit reached)",
        });
      }
      if (data.stderr) {
        lines.push({ kind: "err", text: data.stderr.trimEnd() });
      }
      if (data.rawStdout) {
        lines.push({ kind: "err", text: data.rawStdout.trimEnd() });
      }
      const totalMs = data.timing?.total_ms;
      const tail = `— exit ${data.exitCode ?? "?"}${
        totalMs !== undefined ? ` · ${totalMs.toFixed(2)}ms` : ""
      }`;
      lines.push({ kind: "meta", text: tail });
      setOutput(lines);
    } catch (err) {
      setOutput([
        { kind: "meta", text: banner },
        { kind: "err", text: `network error: ${(err as Error).message}` },
      ]);
    } finally {
      setRunning(false);
    }
  }, [code, backend, version, asi]);

  const share = useCallback(async () => {
    const url = new URL(window.location.href);
    url.search = "";
    url.searchParams.set(
      "share",
      encodeShare({ code, mode: backend, asi, version }),
    );
    const link = url.toString();
    let ok = false;
    try {
      if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(link);
        ok = true;
      }
    } catch {}
    if (ok) setShareTick((t) => t + 1);
  }, [code, backend, asi, version]);

  useEffect(() => {
    if (shareTick === 0) return;
    const id = setTimeout(() => setShareTick(0), 1500);
    return () => clearTimeout(id);
  }, [shareTick]);

  const syncScroll = () => {
    if (taRef.current && hlRef.current) {
      hlRef.current.scrollTop = taRef.current.scrollTop;
      hlRef.current.scrollLeft = taRef.current.scrollLeft;
    }
  };

  const lineCount = code.split("\n").length;
  const gutter = Array.from({ length: lineCount }, (_, i) =>
    String(i + 1),
  ).join("\n");

  const acceptGhost = () => {
    if (!ghost) return false;
    const next = code.slice(0, caret) + ghost + code.slice(caret);
    setCode(next);
    const newCaret = caret + ghost.length;
    setCaret(newCaret);
    requestAnimationFrame(() => {
      if (taRef.current) {
        taRef.current.selectionStart = taRef.current.selectionEnd = newCaret;
        taRef.current.focus();
      }
    });
    return true;
  };

  return (
    <div className="pg-shell pg-full">
      <div className="pg-toolbar">
        <label className="pg-toolbar-label" htmlFor={exampleId}>
          Example
        </label>
        <select
          id={exampleId}
          className="pg-select min-w-[260px]"
          value={exId}
          onChange={(e) => setExId(e.target.value)}
        >
          {EXAMPLES.map((e) => (
            <option key={e.id} value={e.id}>
              {e.label}
            </option>
          ))}
        </select>

        <label className="pg-toolbar-label ml-3" htmlFor={backendId}>
          Backend
        </label>
        <select
          id={backendId}
          className="pg-select min-w-[150px]"
          value={backend}
          onChange={(e) => setBackend(e.target.value as Backend)}
        >
          <option value="tree-walk">tree-walk</option>
          <option value="bytecode">bytecode VM</option>
        </select>

        <label className="pg-toolbar-label ml-3" htmlFor={asiId}>
          ASI
        </label>
        <label
          className="pg-asi-toggle"
          htmlFor={asiId}
          title="Automatic semicolon insertion (--asi)"
        >
          <input
            id={asiId}
            type="checkbox"
            checked={asi}
            onChange={(e) => setAsi(e.target.checked)}
          />
          <span>{asi ? "on" : "off"}</span>
        </label>

        {/* Version selector is **display-only**: the API runs against the
            single bundled `GocciaScriptLoader` binary regardless of choice.
            The selection is reflected in the run banner and round-tripped in
            share links so users can mark which release they tested against. */}
        <label className="pg-toolbar-label ml-3" htmlFor={versionId}>
          Version
        </label>
        <select
          id={versionId}
          className="pg-select min-w-[110px]"
          value={version}
          onChange={(e) => setVersion(e.target.value)}
        >
          {versions.map((v, i) => (
            <option key={v} value={v}>
              {v === "nightly" ? "nightly" : i === 0 ? `${v} · latest` : v}
            </option>
          ))}
        </select>

        <div className="ml-auto flex items-center gap-2">
          <span className="font-mono text-[0.72rem] text-ink-3">⌘ + Enter</span>
          <button
            type="button"
            className="pg-share"
            onClick={share}
            title="Copy a sharable URL of the current code"
          >
            <CopyIcon size={14} />
            {shareTick > 0 ? (
              <span key={shareTick} className="copied-flash">
                link copied
              </span>
            ) : (
              <span>Share</span>
            )}
          </button>
          <button
            type="button"
            className="pg-run"
            disabled={running}
            onClick={run}
          >
            <RunIcon size={14} /> {running ? "Running…" : "Run"}
          </button>
        </div>
      </div>

      <p className="pg-example-desc mx-6 mb-3">{example.desc}</p>

      <div className="pg-panes pg-panes-full">
        <div className="pg-pane">
          <div className="pg-pane-head">
            <TerminalIcon size={14} /> {example.id}.{example.ext ?? "js"}
            <span className="ml-auto text-[0.7rem]">{lineCount} lines</span>
          </div>
          <div className="pg-editor">
            <pre className="pg-gutter">{gutter}</pre>
            <div className="pg-editor-body">
              <pre className="pg-hl" ref={hlRef} aria-hidden="true">
                <code>
                  <HighlightedCode code={code.slice(0, caret)} />
                  {ghost && <span className="pg-ghost">{ghost}</span>}
                  <HighlightedCode code={`${code.slice(caret)}\n`} />
                </code>
              </pre>
              <textarea
                ref={taRef}
                id={editorId}
                aria-label={`GocciaScript editor — ${example.label}`}
                spellCheck={false}
                value={code}
                onChange={(e) => {
                  setCode(e.target.value);
                  setCaret(e.target.selectionStart);
                }}
                onSelect={(e) => setCaret(e.currentTarget.selectionStart)}
                onScroll={syncScroll}
                onKeyDown={(e) => {
                  const target = e.currentTarget;
                  const s = target.selectionStart;
                  const en = target.selectionEnd;
                  const value = target.value;

                  // Tab accepts the ghost when one is showing.
                  if (e.key === "Tab" && ghost) {
                    e.preventDefault();
                    acceptGhost();
                    return;
                  }
                  // Tab without ghost = indent two spaces.
                  if (e.key === "Tab") {
                    e.preventDefault();
                    const next = `${value.slice(0, s)}  ${value.slice(en)}`;
                    setCode(next);
                    requestAnimationFrame(() => {
                      target.selectionStart = target.selectionEnd = s + 2;
                      setCaret(s + 2);
                    });
                    return;
                  }
                  if (e.key === "Escape" && ghost) {
                    // Hide ghost without consuming the key.
                    setCaret(caret);
                  }
                  if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
                    e.preventDefault();
                    run();
                    return;
                  }

                  // ── Auto-indent on Enter ────────────────────────────
                  // Mirrors the leading whitespace of the current line, with
                  // an extra two spaces after an opener (`{`, `[`, `(`).
                  // When the caret sits between a matching opener/closer
                  // (`{|}`, `[|]`, `(|)`), splits into three lines and
                  // places the caret on the indented middle line.
                  if (
                    e.key === "Enter" &&
                    !e.shiftKey &&
                    !e.altKey &&
                    !e.metaKey &&
                    !e.ctrlKey
                  ) {
                    const lineStart = value.lastIndexOf("\n", s - 1) + 1;
                    const indentMatch = value
                      .slice(lineStart, s)
                      .match(/^[ \t]*/);
                    const indent = indentMatch ? indentMatch[0] : "";
                    const prevCh = s > 0 ? value[s - 1] : "";
                    const nextCh = value[s] ?? "";
                    const OPEN_TO_CLOSE: Record<string, string> = {
                      "{": "}",
                      "[": "]",
                      "(": ")",
                    };
                    const opensBlock = prevCh in OPEN_TO_CLOSE;
                    const splitsPair =
                      opensBlock && OPEN_TO_CLOSE[prevCh] === nextCh;
                    const extra = opensBlock ? "  " : "";
                    e.preventDefault();
                    if (splitsPair) {
                      const insert = `\n${indent}${extra}\n${indent}`;
                      const next = value.slice(0, s) + insert + value.slice(en);
                      setCode(next);
                      const caretPos = s + 1 + indent.length + extra.length;
                      requestAnimationFrame(() => {
                        target.selectionStart = target.selectionEnd = caretPos;
                        setCaret(caretPos);
                      });
                      return;
                    }
                    const insert = `\n${indent}${extra}`;
                    const next = value.slice(0, s) + insert + value.slice(en);
                    setCode(next);
                    const caretPos = s + insert.length;
                    requestAnimationFrame(() => {
                      target.selectionStart = target.selectionEnd = caretPos;
                      setCaret(caretPos);
                    });
                    return;
                  }

                  // ── Smart bracket / quote handling ──────────────────
                  // Skip when modifier keys are held — let normal browser
                  // shortcuts (Cmd+A, Ctrl+C, etc.) through untouched.
                  if (e.metaKey || e.ctrlKey || e.altKey) return;
                  const PAIRS: Record<string, string> = {
                    "(": ")",
                    "[": "]",
                    "{": "}",
                    '"': '"',
                    "'": "'",
                    "`": "`",
                  };
                  const CLOSERS = new Set([")", "]", "}", '"', "'", "`"]);
                  // 1. Auto-close: typing an opener inserts the matching
                  //    closer and leaves the caret between them. With a
                  //    selection, wrap it.
                  if (PAIRS[e.key]) {
                    e.preventDefault();
                    const open = e.key;
                    const close = PAIRS[open];
                    // For symmetric quotes, don't auto-close if we're
                    // already next to one (typing `"` after `"foo` should
                    // just close the string).
                    if (open === close) {
                      if (value[s] === open && s === en) {
                        target.selectionStart = target.selectionEnd = s + 1;
                        setCaret(s + 1);
                        return;
                      }
                    }
                    const selected = value.slice(s, en);
                    const next = `${value.slice(0, s)}${open}${selected}${close}${value.slice(en)}`;
                    setCode(next);
                    requestAnimationFrame(() => {
                      const newPos = s + 1 + selected.length;
                      target.selectionStart = s + 1;
                      target.selectionEnd = newPos;
                      setCaret(newPos);
                    });
                    return;
                  }
                  // 2. Close-over: typing a closer when the next character
                  //    is already that closer just steps over it.
                  if (CLOSERS.has(e.key) && s === en && value[s] === e.key) {
                    e.preventDefault();
                    target.selectionStart = target.selectionEnd = s + 1;
                    setCaret(s + 1);
                    return;
                  }
                  // 3. Backspace over an empty pair removes both halves:
                  //    `(|)` + Backspace → `|`.
                  if (e.key === "Backspace" && s === en && s > 0) {
                    const prev = value[s - 1];
                    const nextCh = value[s];
                    if (PAIRS[prev] && PAIRS[prev] === nextCh) {
                      e.preventDefault();
                      const next = value.slice(0, s - 1) + value.slice(s + 1);
                      setCode(next);
                      requestAnimationFrame(() => {
                        target.selectionStart = target.selectionEnd = s - 1;
                        setCaret(s - 1);
                      });
                      return;
                    }
                  }
                }}
              />
            </div>
          </div>
        </div>

        <div className="pg-pane">
          <div className="pg-pane-head">
            <SparkleIcon size={14} /> output
            <span className="ml-auto text-[0.7rem]">{output.length} lines</span>
          </div>
          <ConsolePanel>
            <AnimatedOutput
              // Run id derived from the output identity — bumping the
              // banner / clearing for a new execution remounts the
              // component and replays the line-stagger reveal.
              runKey={output.length === 0 ? "empty" : (output[0]?.text ?? "x")}
              lines={output}
              showCaret={!running ? output.length > 0 : true}
            />
          </ConsolePanel>
        </div>
      </div>
    </div>
  );
}
