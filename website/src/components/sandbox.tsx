"use client";

import { useCallback, useEffect, useRef, useState } from "react";
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

const TOOL_CALL_TASK = "Summarize this month's transactions and find outliers.";

type ToolStep = { tool: string; call: string; role: string };
type ToolCost = {
  calls: number;
  latency: string;
  tokens: string;
  context: string;
};
type ToolFlow = {
  label: string;
  title: string;
  steps: ToolStep[];
  cost: ToolCost;
  risks: string[];
};

const TOOL_CALL_FLOWS: { bash: ToolFlow; goccia: ToolFlow } = {
  bash: {
    label: "Bash + jq",
    title: "5 tool calls · ~14kB context",
    steps: [
      { tool: "bash", call: "ls /tmp/agent/transactions/", role: "discover" },
      {
        tool: "bash",
        call: "cat /tmp/agent/transactions/2026-04.json",
        role: "load",
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add' 2026-04.json",
        role: "sum",
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add / length' 2026-04.json",
        role: "average",
      },
      {
        tool: "bash",
        call: "jq '[.[] | select(.amount > 280)]' 2026-04.json",
        role: "outliers",
      },
    ],
    cost: {
      calls: 5,
      latency: "~1.8s",
      tokens: "~3,420 tok",
      context: "~14kB",
    },
    risks: [
      "values cross 5 process boundaries — possible to lose precision or quoting",
      "agent has to remember intermediate state across every call",
      "5 round-trips ≈ 5× the prompt overhead and 5× the chance of a misstep",
    ],
  },
  goccia: {
    label: "GocciaScript (single call)",
    title: "1 tool call · 1 sandbox · ~2kB context",
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
      },
    ],
    cost: { calls: 1, latency: "~4ms", tokens: "~510 tok", context: "~2kB" },
    risks: [
      "all values stay in one sandbox — no serialization between steps",
      "host gets a single structured JSON result back",
    ],
  },
};

function ToolCallComparison() {
  const ref = useRef<HTMLDivElement>(null);
  const [armed, setArmed] = useState(false);
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
    <div className="tcc-grid" ref={ref} data-armed={armed}>
      {(Object.entries(TOOL_CALL_FLOWS) as [string, ToolFlow][]).map(
        ([key, flow]) => {
          const isGoccia = key === "goccia";
          return (
            <div
              key={key}
              className={`tcc-flow${isGoccia ? " tcc-flow-goccia" : ""}`}
            >
              <div className="tcc-head">
                <div>
                  <div className="tcc-label">{flow.label}</div>
                  <h4 className="tcc-title">{flow.title}</h4>
                </div>
                <div className="tcc-cost">
                  <span>
                    <strong>{flow.cost.calls}</strong> call
                    {flow.cost.calls > 1 ? "s" : ""}
                  </span>
                  <span>{flow.cost.latency}</span>
                  <span>{flow.cost.tokens}</span>
                  <span className="tcc-cost-sub">{flow.cost.context}</span>
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
                      </div>
                      <pre className="tcc-call">
                        <code>
                          {isGoccia ? (
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
  prompt: "Summarize this month's transactions and find outliers.",
  tools: {
    run_code: tool({
      description: "Execute GocciaScript in a sandbox",
      inputSchema: z.object({ code: z.string(), globals: z.record(z.any()).optional() }),
      execute: ({ code, globals }) => sandbox.execute(code, { globals }),
    }),
  },
});`;

export function Sandbox() {
  const [code, setCode] = useState(`// Untrusted script from an AI agent.
// The sandbox has no fetch, fs, env, or eval — by default.
const summary = users
  .filter((u) => u.active)
  .map((u) => ({ id: u.id, name: u.name.toUpperCase() }));

console.log("Found", summary.length, "active users");
return { summary, count: summary.length };`);

  const [globalsText, setGlobalsText] = useState(`{
  "users": [
    { "id": 1, "name": "Alice",   "active": true  },
    { "id": 2, "name": "Bob",     "active": false },
    { "id": 3, "name": "Charlie", "active": true  },
    { "id": 4, "name": "Diana",   "active": true  },
    { "id": 5, "name": "Eve",     "active": true  },
    { "id": 6, "name": "Frank",   "active": true  }
  ]
}`);

  const [output, setOutput] = useState<SbLine[]>([]);

  const simulate = useCallback(() => {
    const lines: SbLine[] = [
      {
        kind: "meta",
        text: "› goccia run --timeout=500 --memory=64MB --globals=context.json",
      },
      { kind: "meta", text: "   caps: — none —" },
    ];
    let globalsOk = true;
    try {
      JSON.parse(globalsText);
    } catch {
      globalsOk = false;
    }
    if (!globalsOk) {
      lines.push({
        kind: "err",
        text: "SyntaxError: failed to parse context.json",
      });
    } else if (/\bfetch\s*\(/.test(code)) {
      lines.push({ kind: "err", text: "ReferenceError: fetch is not defined" });
      lines.push({
        kind: "meta",
        text: "  the 'fetch' capability is disabled for this sandbox",
      });
    } else if (/\beval\s*\(/.test(code)) {
      lines.push({
        kind: "err",
        text: "SyntaxError: eval is not allowed in GocciaScript",
      });
    } else if (/require\s*\(/.test(code)) {
      lines.push({
        kind: "err",
        text: "ReferenceError: require is not defined — use ES module imports",
      });
    } else if (/process\./.test(code)) {
      lines.push({
        kind: "err",
        text: "ReferenceError: process is not defined",
      });
    } else if (
      /while\s*\(\s*true/.test(code) ||
      /for\s*\(\s*;\s*;/.test(code)
    ) {
      lines.push({
        kind: "err",
        text: "Aborted: script exceeded 500ms timeout",
      });
    } else {
      lines.push({ kind: "out", text: "{" });
      lines.push({ kind: "out", text: '  "summary": [' });
      lines.push({ kind: "out", text: '    { "id": 1, "name": "ALICE" },' });
      lines.push({ kind: "out", text: '    { "id": 3, "name": "CHARLIE" },' });
      lines.push({ kind: "out", text: '    { "id": 4, "name": "DIANA" },' });
      lines.push({ kind: "out", text: '    { "id": 5, "name": "EVE" },' });
      lines.push({ kind: "out", text: '    { "id": 6, "name": "FRANK" }' });
      lines.push({ kind: "out", text: "  ]," });
      lines.push({ kind: "out", text: '  "count": 5' });
      lines.push({ kind: "out", text: "}" });
      lines.push({
        kind: "meta",
        text: "— exit 0 · 4.2ms · heap 0.3MB / 64MB",
      });
    }
    setOutput(lines);
  }, [code, globalsText]);

  // biome-ignore lint/correctness/useExhaustiveDependencies: run once on mount
  useEffect(() => {
    simulate();
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
              <h4>Try the sandbox</h4>
              <p className="m-0 text-ink-3 text-[0.88rem]">
                Edit the script, change the injected globals, execute — and see
                what the host gets back.
              </p>
            </div>
            <div className="flex gap-2">
              <button type="button" className="pg-run" onClick={simulate}>
                <RunIcon size={14} /> Execute
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

        <div className="mt-12">
          <div className="section-head">
            <div className="section-kicker">Integration</div>
            <AnchorH2 id="integration">Drop it into your agent stack.</AnchorH2>
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
      </div>
    </div>
  );
}
