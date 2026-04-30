"use client";

import { useCallback, useEffect, useId, useRef, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import { AnimatedOutput } from "@/components/animated-output";
import { ConsolePanel } from "@/components/console-panel";
import {
  HighlightedCode,
  HighlightedGeneric,
  HighlightedJson,
  HighlightedShell,
} from "@/components/highlighted-code";
import { HighlightedTextarea } from "@/components/highlighted-textarea";
import {
  ArrowIcon,
  BotIcon,
  RunIcon,
  ShieldIcon,
  SparkleIcon,
  TerminalIcon,
} from "@/components/icons";
import { formatMemorySegments, type MemoryJson } from "@/lib/format-memory";
import {
  buildLlmRequest,
  LLM_CALL_TOKENS,
  TOOL_CALL_FLOWS,
  TOOL_CALL_TASK,
  type ToolFlow,
  type ToolFlowKey,
} from "@/lib/tool-call-comparison";

/** When ready to surface the SDK integration snippet, flip to true.
 *  Hidden until the runtime is mature enough to expose a stable
 *  embedding API. */
const SHOW_INTEGRATION = false;

function ToolCallComparison() {
  const ref = useRef<HTMLDivElement>(null);
  const [armed, setArmed] = useState(false);
  const [showPayload, setShowPayload] = useState(false);
  const payloadToggleId = useId();

  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    const timer = setTimeout(() => setArmed(true), 60);
    const obs = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (entry.isIntersecting) setArmed(true);
        }
      },
      { threshold: 0.18 },
    );
    obs.observe(el);
    return () => {
      clearTimeout(timer);
      obs.disconnect();
    };
  }, []);

  return (
    <>
      <div className="tcc-toggle">
        <label htmlFor={payloadToggleId} className="tcc-toggle-label">
          <input
            id={payloadToggleId}
            type="checkbox"
            checked={showPayload}
            onChange={(e) => setShowPayload(e.target.checked)}
          />
          <span>Show Responses API request body</span>
        </label>
        <span className="tcc-toggle-note">
          Each step expands to the request body for that turn — instructions,
          tool definitions, prior function calls, and function-call outputs.
          Token counts via{" "}
          <a
            href="https://github.com/niieani/gpt-tokenizer"
            target="_blank"
            rel="noopener noreferrer"
          >
            gpt-tokenizer
          </a>{" "}
          on GPT-5-family <code>o200k_base</code> tokenization.
        </span>
      </div>
      <div className="tcc-grid" ref={ref} data-armed={armed}>
        {(Object.entries(TOOL_CALL_FLOWS) as [string, ToolFlow][]).map(
          ([key, flow]) => {
            const isGoccia = key === "goccia";
            const flowKey = key as ToolFlowKey;
            const stepTokens = LLM_CALL_TOKENS[flowKey];
            const totalIn = stepTokens.in.reduce((s, t) => s + t, 0);
            const totalOut = stepTokens.out.reduce((s, t) => s + t, 0);
            return (
              <div
                key={key}
                className={`tcc-flow${isGoccia ? " tcc-flow-goccia" : ""}`}
              >
                <div className="tcc-head">
                  <div>
                    <div className="tcc-label">{flow.label}</div>
                    <h4 className="tcc-title">
                      {flow.steps.length} tool call
                      {flow.steps.length === 1 ? "" : "s"}{" "}
                      <span className="tcc-tokens-pair">
                        <span className="tcc-tokens-in">↓ {totalIn} in</span>{" "}
                        <span className="tcc-tokens-out">↑ {totalOut} out</span>
                      </span>
                    </h4>
                  </div>
                </div>
                <ol className="tcc-steps">
                  {flow.steps.map((s, i) => (
                    <li
                      key={i}
                      className="tcc-step"
                      style={{ animationDelay: `${0.18 + i * 0.22}s` }}
                    >
                      <span className="tcc-step-num">{i + 1}</span>
                      <div className="tcc-step-body">
                        <div className="tcc-step-head">
                          <span className="tcc-tool">{s.tool}</span>
                          <span className="tcc-role">{s.role}</span>
                          <span className="tcc-step-tokens">
                            <span className="tcc-tokens-in">
                              ↓ {stepTokens.in[i]}
                            </span>{" "}
                            <span className="tcc-tokens-out">
                              ↑ {stepTokens.out[i]}
                            </span>
                          </span>
                        </div>
                        <pre
                          className="tcc-call"
                          data-payload={showPayload ? "true" : "false"}
                        >
                          <code>
                            {showPayload ? (
                              <HighlightedJson
                                code={JSON.stringify(
                                  buildLlmRequest(flowKey, i),
                                  null,
                                  2,
                                )}
                              />
                            ) : isGoccia ? (
                              <HighlightedCode code={s.call} />
                            ) : (
                              <HighlightedShell code={s.call} />
                            )}
                          </code>
                        </pre>
                      </div>
                    </li>
                  ))}
                </ol>
                <ul
                  className="tcc-risks"
                  style={{
                    animationDelay: `${0.25 + flow.steps.length * 0.22}s`,
                  }}
                >
                  {flow.risks.map((r, i) => (
                    <li
                      key={i}
                      className={isGoccia ? "tcc-risk-good" : "tcc-risk-bad"}
                    >
                      <span className="tcc-risk-mark">
                        {isGoccia ? "✓" : "·"}
                      </span>{" "}
                      {r}
                    </li>
                  ))}
                </ul>
              </div>
            );
          },
        )}
      </div>
    </>
  );
}

type SbLine = { kind: "meta" | "out" | "err"; text: string };

const VERCEL_EXAMPLE = `import { generateText, tool } from "ai";
import { openai } from "@ai-sdk/openai";
import { z } from "zod";
import { Goccia } from "@goccia/runtime";

const sandbox = new Goccia({ timeout: 500, memory: 64 << 20 });

await generateText({
  model: openai("gpt-5"),
  prompt: "Summarize the latest transaction batch and find outliers.",
  tools: {
    run_code: tool({
      description: "Execute GocciaScript in a sandbox",
      inputSchema: z.object({ code: z.string(), globals: z.record(z.any()).optional() }),
      execute: ({ code, globals }) => sandbox.execute(code, { globals }),
    }),
  },
});`;

/** Default sandbox script — keep the output to ~3 short lines so the
 *  host-result panel doesn't dominate the page layout. The runner
 *  doesn't accept top-level `return` (source treated as a module),
 *  so we use `console.log` for the visible output. */
const DEFAULT_CODE = `// Untrusted script from an AI agent.
const active = users.filter((u) => u.active);
const names = active
  .map((u) => u.name.toUpperCase())
  .join(", ");
console.log("active count:", active.length);
console.log("names:", names);
console.log("first id:", active[0]?.id);`;

const DEFAULT_GLOBALS = `{
  "users": [{
    "id": 1, "name": "Alice",
    "active": true
  }, {
    "id": 2, "name": "Bob",
    "active": false
  }, {
    "id": 3, "name": "Charlie",
    "active": true
  }]
}`;

const MIN_PANE_SIZE = 0.65;
const KEYBOARD_RESIZE_STEP = 0.12;

// Reject globals keys that aren't valid identifiers up front — `const "user-id"
// = …` and `const 123 = …` are both syntax errors that would otherwise blow up
// the runner's parser before any of the user's actual script ran. Matching the
// IdentifierStart / IdentifierPart subset we accept (ASCII only — Unicode
// identifier characters are valid JS but a bigger surface for confusables in
// a sandboxed-paste UI).
const IDENTIFIER_RE = /^[A-Za-z_$][A-Za-z0-9_$]*$/;

// Reserved words that pass the identifier regex above but are still illegal
// as `const` binding names. Covers the unconditionally reserved set plus the
// strict-mode-only future-reserved words (we run user code in module scope,
// which is implicitly strict). `await` is in here because GocciaScript
// modules are top-level-async-aware and binding `await` is a SyntaxError in
// module goal symbol. Keep this in sync with the engine's parser if either
// side adds a keyword.
const RESERVED_BINDING_WORDS = new Set([
  "await",
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "null",
  "return",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "yield",
  "static",
  "implements",
  "interface",
  "package",
  "private",
  "protected",
  "public",
]);

/** System prompt prepended to every sandbox execution so the full
 *  source (visible in tool-call logs) explains what GocciaScript is. */
const GOCCIA_SYSTEM_PROMPT = `\
// ─── system ───
// GocciaScript is a strict ECMAScript subset, sandboxed by default.
// No eval, no dynamic import, no fs, no env, no ambient globals.
// No var, no function declarations, no loose equality (== / !=).
// Only the injected globals below are available to the script.`;

/** Build the script that actually runs: a system prompt header,
 *  `const` declarations for each globals key, and the user's code.
 *  Throws on globals keys that aren't valid JS identifiers. */
function buildScript(userCode: string, parsedGlobals: Record<string, unknown>) {
  const prelude = Object.entries(parsedGlobals)
    .map(([k, v]) => {
      if (!IDENTIFIER_RE.test(k)) {
        throw new Error(
          `globals key "${k}" is not a valid JavaScript identifier`,
        );
      }
      if (RESERVED_BINDING_WORDS.has(k)) {
        throw new Error(
          `globals key "${k}" is a reserved word and can't be a const name`,
        );
      }
      return `const ${k} = ${JSON.stringify(v)};`;
    })
    .join("\n");
  const globalsBlock = prelude
    ? `\n\n// ─── injected globals ───\n${prelude}`
    : "";
  return `${GOCCIA_SYSTEM_PROMPT}${globalsBlock}\n\n// ─── user script ───\n${userCode}`;
}

export function Sandbox() {
  const [code, setCode] = useState(DEFAULT_CODE);
  const [globalsText, setGlobalsText] = useState(DEFAULT_GLOBALS);
  const [output, setOutput] = useState<SbLine[]>([]);
  const [running, setRunning] = useState(false);
  // Bumped on each new execution so `<AnimatedOutput>` re-mounts and
  // re-runs its line-stagger reveal cleanly.
  const [runId, setRunId] = useState(0);
  const [paneCols, setPaneCols] = useState<[number, number, number]>([
    1.3, 0.9, 1,
  ]);

  const resizePanePair = useCallback(
    (
      handleIndex: 0 | 1,
      mode:
        | { kind: "set"; value: number }
        | { kind: "step"; direction: -1 | 1 }
        | { kind: "edge"; side: "start" | "end" },
    ) => {
      setPaneCols((current) => {
        const next = [...current] as [number, number, number];
        const leftIndex = handleIndex;
        const rightIndex = handleIndex + 1;
        const combined = current[leftIndex] + current[rightIndex];
        let nextLeft = current[leftIndex];

        if (mode.kind === "set") {
          nextLeft = mode.value;
        } else if (mode.kind === "step") {
          nextLeft += mode.direction * KEYBOARD_RESIZE_STEP;
        } else {
          nextLeft =
            mode.side === "start" ? MIN_PANE_SIZE : combined - MIN_PANE_SIZE;
        }

        nextLeft = Math.min(
          Math.max(nextLeft, MIN_PANE_SIZE),
          combined - MIN_PANE_SIZE,
        );
        next[leftIndex] = nextLeft;
        next[rightIndex] = combined - nextLeft;
        return next;
      });
    },
    [],
  );

  const execute = useCallback(async () => {
    // Validate globals JSON up-front so a typo there doesn't cost us a
    // round-trip to the runner.
    let parsedGlobals: Record<string, unknown>;
    try {
      const raw = JSON.parse(globalsText);
      if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
        throw new Error("globals must be a JSON object");
      }
      parsedGlobals = raw as Record<string, unknown>;
    } catch (err) {
      setOutput([
        {
          kind: "meta",
          text: "GocciaScriptLoader --timeout=500 --globals=context.json",
        },
        {
          kind: "err",
          text: `SyntaxError: ${err instanceof Error ? err.message : String(err)}`,
        },
        {
          kind: "meta",
          text: "  fix the JSON in `context.json` and re-run.",
        },
      ]);
      return;
    }

    let fullCode: string;
    try {
      fullCode = buildScript(code, parsedGlobals);
    } catch (err) {
      // `buildScript` throws on globals keys that aren't valid JS
      // identifiers. Surface the message inline rather than 500ing the
      // runner with a parse error.
      setOutput([
        {
          kind: "meta",
          text: "GocciaScriptLoader --timeout=500 --globals=context.json",
        },
        {
          kind: "err",
          text: `SyntaxError: ${err instanceof Error ? err.message : String(err)}`,
        },
      ]);
      return;
    }
    const banner: SbLine = {
      kind: "meta",
      text: "GocciaScriptLoader --timeout=500 --globals=context.json",
    };
    setRunning(true);
    setRunId((r) => r + 1);
    setOutput([banner]);

    try {
      const res = await fetch("/api/execute", {
        method: "POST",
        headers: { "content-type": "application/json" },
        // `mode: "interpreted"` matches the route's accepted values
        // (`"interpreted" | "bytecode"`) — see `/api/execute/route.ts`.
        body: JSON.stringify({
          code: fullCode,
          mode: "interpreted",
          asi: true,
        }),
      });
      if (res.status === 429) {
        const retry = res.headers.get("Retry-After") ?? "60";
        setOutput([
          banner,
          {
            kind: "err",
            text: `Rate limit exceeded — try again in ${retry}s.`,
          },
        ]);
        return;
      }
      // `.json()` on a non-2xx body that isn't valid JSON (gateway HTML
      // page, empty body) would throw and bounce us into the network-error
      // path with a confusing message — fall back to an empty object so
      // the response-status branch below renders the real story.
      const data = (await res.json().catch(() => ({}))) as {
        ok?: boolean;
        output?: string;
        value?: unknown;
        error?: { message: string; line?: number | null } | null;
        timing?: { total_ms: number };
        memory?: MemoryJson | null;
      };

      // Non-2xx responses from `/api/execute` always include a structured
      // `error`-shape payload (see `transportError` in the route), so we
      // surface that message rather than synthesising a fake `exit 0`.
      if (!res.ok) {
        const where = data.error?.line ? ` (line ${data.error.line})` : "";
        setOutput([
          banner,
          {
            kind: "err",
            text: data.error
              ? `${data.error.message}${where}`
              : `Request failed: HTTP ${res.status} ${res.statusText}`,
          },
        ]);
        return;
      }

      const lines: SbLine[] = [banner];
      if (data.error) {
        const where = data.error.line ? ` (line ${data.error.line})` : "";
        lines.push({
          kind: "err",
          text: `${data.error.message}${where}`,
        });
      } else {
        if (data.output) {
          for (const line of data.output.replace(/\n$/, "").split("\n")) {
            lines.push({ kind: "out", text: line });
          }
        }
        if (data.value !== null && data.value !== undefined) {
          // Pretty-print the structured value the host receives — that's
          // the headline output of the demo.
          const pretty = JSON.stringify(data.value, null, 2);
          for (const line of pretty.split("\n")) {
            lines.push({ kind: "out", text: line });
          }
        }
        const totalMs = data.timing?.total_ms;
        lines.push({
          kind: "meta",
          text: `— exit 0${
            totalMs !== undefined ? ` · ${totalMs.toFixed(2)}ms` : ""
          }${formatMemorySegments(data.memory)}`,
        });
      }
      setOutput(lines);
    } catch (err) {
      setOutput([
        banner,
        {
          kind: "err",
          text: `network error: ${err instanceof Error ? err.message : String(err)}`,
        },
      ]);
    } finally {
      setRunning(false);
    }
  }, [code, globalsText]);

  const startPaneResize = useCallback(
    (handleIndex: 0 | 1) => (event: React.PointerEvent<HTMLElement>) => {
      event.preventDefault();
      const el = event.currentTarget;
      const pointerId = event.pointerId;
      const container = el.closest(".sb-demo-body") as HTMLElement | null;
      if (!container) return;
      const { left, width } = container.getBoundingClientRect();
      const startColumns = paneCols;
      const total = startColumns[0] + startColumns[1] + startColumns[2];

      el.setPointerCapture(pointerId);
      const move = (moveEvent: PointerEvent) => {
        const x = Math.min(Math.max(moveEvent.clientX - left, 0), width);
        if (handleIndex === 0) {
          const combined = startColumns[0] + startColumns[1];
          resizePanePair(handleIndex, {
            kind: "set",
            value: Math.min(
              Math.max((x / width) * total, MIN_PANE_SIZE),
              combined - MIN_PANE_SIZE,
            ),
          });
          return;
        }

        const beforeThird = Math.min(Math.max((x / width) * total, 0), total);
        const combined = startColumns[1] + startColumns[2];
        resizePanePair(handleIndex, {
          kind: "set",
          value: Math.min(
            Math.max(beforeThird - startColumns[0], MIN_PANE_SIZE),
            combined - MIN_PANE_SIZE,
          ),
        });
      };
      const cleanup = () => {
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", cleanup);
        window.removeEventListener("pointercancel", cleanup);
        el.removeEventListener("lostpointercapture", cleanup);
        if (el.hasPointerCapture(pointerId)) {
          el.releasePointerCapture(pointerId);
        }
      };
      window.addEventListener("pointermove", move);
      window.addEventListener("pointerup", cleanup, { once: true });
      window.addEventListener("pointercancel", cleanup, { once: true });
      el.addEventListener("lostpointercapture", cleanup, { once: true });
    },
    [paneCols, resizePanePair],
  );

  const handlePaneResizeKeyDown = useCallback(
    (handleIndex: 0 | 1) => (event: React.KeyboardEvent<HTMLElement>) => {
      if (
        event.key !== "ArrowLeft" &&
        event.key !== "ArrowRight" &&
        event.key !== "ArrowUp" &&
        event.key !== "ArrowDown" &&
        event.key !== "Home" &&
        event.key !== "End"
      ) {
        return;
      }

      event.preventDefault();
      if (event.key === "Home") {
        resizePanePair(handleIndex, { kind: "edge", side: "start" });
        return;
      }
      if (event.key === "End") {
        resizePanePair(handleIndex, { kind: "edge", side: "end" });
        return;
      }

      resizePanePair(handleIndex, {
        kind: "step",
        direction:
          event.key === "ArrowLeft" || event.key === "ArrowUp" ? -1 : 1,
      });
    },
    [resizePanePair],
  );

  // biome-ignore lint/correctness/useExhaustiveDependencies: run once on mount
  useEffect(() => {
    execute();
  }, []);

  const vercelLineCount = VERCEL_EXAMPLE.split("\n").length;

  return (
    <div className="pt-16 pb-24">
      <div className="container">
        <div className="section-head">
          <div className="section-kicker">Sandbox · AI agents</div>
          <AnchorH2 id="overview">
            A runtime designed for{" "}
            <span className="wave-under">code you didn&apos;t write</span>.
          </AnchorH2>
          <p>
            GocciaScript is meant for running untrusted or user-provided code in
            a host-controlled environment. Scripts receive only the data and
            capabilities the host provides, run with explicit limits, and return
            structured results that the host can inspect.
          </p>
        </div>

        <div className="sb-diagram">
          <div className="sb-diagram-inner">
            <div className="sb-node">
              <div className="sb-node-icon">
                <BotIcon size={20} />
              </div>
              <h4>AI agent</h4>
              <p>LLM emits GocciaScript via a tool call</p>
            </div>
            <div className="sb-arrow">
              <ArrowIcon size={22} />
            </div>
            <div className="sb-node center">
              <div className="sb-node-icon">
                <ShieldIcon size={20} />
              </div>
              <h4>Goccia sandbox</h4>
              <p>explicit globals · capability gates · timeout · memory cap</p>
            </div>
            <div className="sb-arrow">
              <ArrowIcon size={22} />
            </div>
            <div className="sb-node">
              <div className="sb-node-icon">
                <SparkleIcon size={20} />
              </div>
              <h4>Structured result</h4>
              <p>JSON</p>
            </div>
          </div>
        </div>

        <div className="mt-12">
          <div className="section-head">
            <div className="section-kicker">Tool-call comparison</div>
            <AnchorH2 id="tool-call-comparison">
              One sandboxed call vs five.
            </AnchorH2>
            <p>
              The same agent task —{" "}
              <em className="italic">&ldquo;{TOOL_CALL_TASK}&rdquo;</em> —
              solved with a typical tool stack and again with a single
              GocciaScript call. This comparison assumes each tool call depends
              on data discovered or produced by the previous step, so the calls
              cannot be parallelized without changing the task shape.
            </p>
          </div>
          <ToolCallComparison />
        </div>

        <div className="sb-demo mt-12">
          <div className="sb-demo-head">
            <div>
              <h4>Sandbox preview</h4>
              <p className="m-0 text-ink-3 text-[0.88rem]">
                Edit the script or globals and hit <strong>Execute</strong> —
                the code runs in the same sandboxed runtime used by the server
                preview and returns a structured host result.
              </p>
            </div>
            <div className="flex gap-2">
              <button
                type="button"
                className="pg-run"
                onClick={execute}
                disabled={running}
              >
                <RunIcon size={14} /> {running ? "Running…" : "Execute"}
              </button>
            </div>
          </div>

          <div
            className="sb-demo-body sb-demo-body-3col"
            style={
              {
                "--sb-col-1": `${paneCols[0]}fr`,
                "--sb-col-2": `${paneCols[1]}fr`,
                "--sb-col-3": `${paneCols[2]}fr`,
              } as React.CSSProperties
            }
          >
            <div className="sb-field">
              <div className="sb-field-label">
                sandboxed script{" "}
                <span className="text-ink-3 font-normal">· script.js</span>
              </div>
              <HighlightedTextarea
                className="sb-textarea-hl"
                value={code}
                onChange={setCode}
                language="goccia"
                lineNumbers
              />
            </div>
            {/* biome-ignore lint/a11y/useSemanticElements: this is an interactive splitter; button keeps pointer dragging reliable while ARIA exposes separator semantics. */}
            <button
              type="button"
              className="sb-resizer"
              role="separator"
              tabIndex={0}
              aria-orientation="vertical"
              aria-label="Resize script and globals panes"
              aria-valuemin={Math.round(
                (MIN_PANE_SIZE / (paneCols[0] + paneCols[1])) * 100,
              )}
              aria-valuemax={Math.round(
                ((paneCols[0] + paneCols[1] - MIN_PANE_SIZE) /
                  (paneCols[0] + paneCols[1])) *
                  100,
              )}
              aria-valuenow={Math.round(
                (paneCols[0] / (paneCols[0] + paneCols[1])) * 100,
              )}
              onPointerDown={startPaneResize(0)}
              onKeyDown={handlePaneResizeKeyDown(0)}
            />
            <div className="sb-field">
              <div className="sb-field-label">
                globals{" "}
                <span className="text-ink-3 font-normal">· context.json</span>
              </div>
              <HighlightedTextarea
                className="sb-textarea-hl"
                value={globalsText}
                onChange={setGlobalsText}
                language="json"
                lineNumbers
              />
            </div>
            {/* biome-ignore lint/a11y/useSemanticElements: this is an interactive splitter; button keeps pointer dragging reliable while ARIA exposes separator semantics. */}
            <button
              type="button"
              className="sb-resizer"
              role="separator"
              tabIndex={0}
              aria-orientation="vertical"
              aria-label="Resize globals and result panes"
              aria-valuemin={Math.round(
                (MIN_PANE_SIZE / (paneCols[1] + paneCols[2])) * 100,
              )}
              aria-valuemax={Math.round(
                ((paneCols[1] + paneCols[2] - MIN_PANE_SIZE) /
                  (paneCols[1] + paneCols[2])) *
                  100,
              )}
              aria-valuenow={Math.round(
                (paneCols[1] / (paneCols[1] + paneCols[2])) * 100,
              )}
              onPointerDown={startPaneResize(1)}
              onKeyDown={handlePaneResizeKeyDown(1)}
            />
            <div className="sb-field">
              <div className="sb-field-label">host result</div>
              <ConsolePanel className="flex-1 rounded-t-none">
                <AnimatedOutput
                  runKey={runId}
                  lines={output.map((l) => ({
                    kind:
                      l.kind === "meta"
                        ? "meta"
                        : l.kind === "err"
                          ? "err"
                          : "out",
                    text: l.text,
                  }))}
                />
              </ConsolePanel>
            </div>
          </div>
        </div>

        {/* Integration is hidden until the embedding API stabilises and the
            `@goccia/runtime` package is published. The snippet stays so
            unhiding is a one-flag flip. */}
        {SHOW_INTEGRATION && (
          <div className="mt-12">
            <div className="section-head">
              <div className="section-kicker">Integration</div>
              <AnchorH2 id="integration">
                Drop it into your agent stack.
              </AnchorH2>
              <p>
                GocciaScript is a Vercel AI SDK tool away — register it once and
                let your agent run code in a sandbox you control.
              </p>
            </div>

            <div className="code-card">
              <div className="code-card-head">
                <TerminalIcon size={14} />
                <span>agent.ts · Vercel AI SDK</span>
                <span className="ml-auto text-[0.7rem] text-ink-3">
                  {vercelLineCount} lines
                </span>
              </div>
              <pre className="code-card-body">
                <code>
                  <HighlightedGeneric code={VERCEL_EXAMPLE} language="ts" />
                </code>
              </pre>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
