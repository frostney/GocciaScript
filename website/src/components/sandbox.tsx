"use client";

import { useCallback, useEffect, useId, useRef, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import {
  HighlightedCode,
  HighlightedGeneric,
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

const TOOL_CALL_TASK =
  "Summarize the latest transaction batch and find outliers.";

/** When ready to surface the SDK integration snippet, flip to true.
 *  Hidden until the runtime is mature enough to expose a stable
 *  embedding API. */
const SHOW_INTEGRATION = false;

type ToolStep = {
  tool: string;
  /** Single argument value for the OpenAI-style tool call. The
   *  `argName` key on the parent flow names what JSON property
   *  it lives under (`command` for bash, `code` for run_code, …). */
  call: string;
  role: string;
  /** Pre-computed tiktoken count for the OpenAI tool-call envelope:
   *  `{"name":"<tool>","arguments":"<JSON-string of args>"}`.
   *  Verified once with `gpt-tokenizer` on the cl100k_base encoder
   *  (used by GPT-4 / GPT-4o); update if the call text changes. */
  tokens: number;
};

type ToolFlow = {
  label: string;
  argName: "command" | "code";
  steps: ToolStep[];
  risks: string[];
};

const TOOL_CALL_FLOWS: { bash: ToolFlow; goccia: ToolFlow } = {
  bash: {
    label: "Bash + jq",
    argName: "command",
    steps: [
      {
        tool: "bash",
        call: "ls /tmp/agent/transactions/",
        role: "discover",
        tokens: 21,
      },
      {
        tool: "bash",
        call: "cat /tmp/agent/transactions/transactions.current.json",
        role: "load",
        tokens: 24,
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add' transactions.current.json",
        role: "sum",
        tokens: 25,
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add / length' transactions.current.json",
        role: "average",
        tokens: 27,
      },
      {
        tool: "bash",
        call: "jq '[.[] | select(.amount > 280)]' transactions.current.json",
        role: "outliers",
        tokens: 29,
      },
    ],
    risks: [
      "values cross 5 process boundaries — possible to lose precision or quoting",
      "agent has to remember intermediate state across every call",
      "5 round-trips ≈ 5× the prompt overhead and 5× the chance of a misstep",
    ],
  },
  goccia: {
    label: "GocciaScript (single call)",
    argName: "code",
    steps: [
      {
        tool: "run_code",
        call: `const total = transactions.reduce((s, t) => s + t.amount, 0);
const avg = total / transactions.length;
const stdev = Math.sqrt(
  transactions.reduce((s, t) => s + (t.amount - avg) ** 2, 0) / transactions.length
);
const outliers = transactions.filter((t) => Math.abs(t.amount - avg) > 2 * stdev);
({ total, avg, outliers });`,
        role: "everything",
        tokens: 118,
      },
    ],
    risks: [
      "all values stay in one sandbox — no serialization between steps",
      "host gets a single structured JSON result back",
    ],
  },
};

/** Build the canonical OpenAI tool-call envelope for one step:
 *
 *   {"name":"<tool>","arguments":"<JSON-string of args>"}
 *
 * The model pays tokens for the entire envelope, not just the inner
 * command — that's what the toggle reveals. */
function toolCallPayload(step: ToolStep, argName: ToolFlow["argName"]): string {
  return JSON.stringify(
    { name: step.tool, arguments: JSON.stringify({ [argName]: step.call }) },
    null,
    2,
  );
}

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
          <span>Show tool-call payload</span>
        </label>
        <span className="tcc-toggle-note">
          Token counts via{" "}
          <a
            href="https://github.com/openai/tiktoken"
            target="_blank"
            rel="noopener noreferrer"
          >
            tiktoken
          </a>{" "}
          on the <code>cl100k_base</code> encoder (GPT-4).
        </span>
      </div>
      <div className="tcc-grid" ref={ref} data-armed={armed}>
        {(Object.entries(TOOL_CALL_FLOWS) as [string, ToolFlow][]).map(
          ([key, flow]) => {
            const isGoccia = key === "goccia";
            const totalTokens = flow.steps.reduce(
              (s, step) => s + step.tokens,
              0,
            );
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
                      {flow.steps.length === 1 ? "" : "s"} · {totalTokens} tok
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
                            {s.tokens} tok
                          </span>
                        </div>
                        <pre className="tcc-call">
                          <code>
                            {showPayload ? (
                              <HighlightedGeneric
                                code={toolCallPayload(s, flow.argName)}
                                language="json"
                              />
                            ) : isGoccia ? (
                              <HighlightedCode code={s.call} />
                            ) : (
                              s.call
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

/** Default sandbox script — ends with an expression statement so the
 *  runner returns it as `value`. (The runner doesn't allow `return`
 *  at top level since the source is treated as a module.) */
const DEFAULT_CODE = `// Untrusted script from an AI agent.
// The sandbox has no fetch, fs, env, or eval — by default.
const summary = users
  .filter((u) => u.active)
  .map((u) => ({ id: u.id, name: u.name.toUpperCase() }));

console.log("Found", summary.length, "active users");
({ summary, count: summary.length });`;

const DEFAULT_GLOBALS = `{
  "users": [
    { "id": 1, "name": "Alice",   "active": true  },
    { "id": 2, "name": "Bob",     "active": false },
    { "id": 3, "name": "Charlie", "active": true  },
    { "id": 4, "name": "Diana",   "active": true  },
    { "id": 5, "name": "Eve",     "active": true  },
    { "id": 6, "name": "Frank",   "active": true  }
  ]
}`;

/** Build the script that actually runs: each top-level globals key
 *  becomes a `const X = <JSON literal>;` declaration above the user's
 *  code. `JSON.stringify` of any JSON value is also a valid JS literal,
 *  so this round-trips cleanly without ad-hoc escaping. */
function buildScript(userCode: string, parsedGlobals: Record<string, unknown>) {
  const prelude = Object.entries(parsedGlobals)
    .map(([k, v]) => `const ${k} = ${JSON.stringify(v)};`)
    .join("\n");
  if (!prelude) return userCode;
  return `// ─── injected globals ───\n${prelude}\n\n// ─── user script ───\n${userCode}`;
}

export function Sandbox() {
  const [code, setCode] = useState(DEFAULT_CODE);
  const [globalsText, setGlobalsText] = useState(DEFAULT_GLOBALS);
  const [output, setOutput] = useState<SbLine[]>([]);
  const [running, setRunning] = useState(false);

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
          text: "› goccia run --timeout=500 --memory=64MB --globals=context.json",
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

    const fullCode = buildScript(code, parsedGlobals);
    const banner: SbLine = {
      kind: "meta",
      text: "› goccia run --timeout=500 --memory=64MB --globals=context.json",
    };
    setRunning(true);
    setOutput([banner, { kind: "meta", text: "  caps: — none —" }]);

    try {
      const res = await fetch("/api/run", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ code: fullCode, mode: "tree-walk", asi: true }),
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
      const data = (await res.json()) as {
        ok?: boolean;
        output?: string;
        value?: unknown;
        error?: { message: string; line?: number | null } | null;
        timing?: { total_ms: number };
      };

      const lines: SbLine[] = [
        banner,
        { kind: "meta", text: "  caps: — none —" },
      ];
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
          text:
            totalMs !== undefined
              ? `— exit 0 · ${totalMs.toFixed(2)}ms`
              : "— exit 0",
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
            GocciaScript was built to execute code produced by agents and
            language models. There is no{" "}
            <code className="font-mono bg-paper-2 px-[0.3em] py-[0.05em] rounded">
              eval
            </code>
            , no synchronous filesystem access, no ambient globals — and every
            capability the host exposes is opt-in.
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
              <p>JSON value + stdout back to the host</p>
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
              GocciaScript call.
            </p>
          </div>
          <ToolCallComparison />
        </div>

        <div className="sb-demo mt-12">
          <div className="sb-demo-head">
            <div>
              <h4>Sandbox preview</h4>
              <p className="m-0 text-ink-3 text-[0.88rem]">
                Edit the script or the injected globals and hit{" "}
                <strong>Execute</strong> — the code runs through the real{" "}
                <code>GocciaScriptLoader</code> binary on the server, with the
                same caps and limits the agent integration would see (no fetch,
                no fs, 500 ms timeout, 64 MB heap).
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

          <div className="sb-demo-body sb-demo-body-3col">
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
              />
            </div>
            <div className="sb-field">
              <div className="sb-field-label">
                injected globals{" "}
                <span className="text-ink-3 font-normal">· context.json</span>
              </div>
              <HighlightedTextarea
                className="sb-textarea-hl"
                value={globalsText}
                onChange={setGlobalsText}
                language="json"
              />
            </div>
            <div className="sb-field">
              <div className="sb-field-label">host result</div>
              <div className="pg-output flex-1 rounded-t-none">
                {output.map((l, i) => (
                  <div
                    key={i}
                    className={`pg-log-line log-${
                      l.kind === "meta"
                        ? "meta"
                        : l.kind === "err"
                          ? "err"
                          : "out"
                    }`}
                  >
                    <span className="pg-log-gutter">
                      {l.kind === "meta" ? "" : l.kind === "err" ? "✗" : "›"}
                    </span>
                    {l.text}
                  </div>
                ))}
              </div>
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
