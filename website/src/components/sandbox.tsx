"use client";

import { useCallback, useEffect, useId, useRef, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import { AnimatedOutput } from "@/components/animated-output";
import { ConsolePanel } from "@/components/console-panel";
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
  /** What the tool returns to the model after this step — added to
   *  the conversation history before the next call, so it inflates
   *  the next request's token cost. */
  result: string;
};

type ToolFlow = {
  label: string;
  argName: "command" | "code";
  toolDef: ToolDef;
  steps: ToolStep[];
  risks: string[];
};

type ToolDef = {
  type: "function";
  function: {
    name: string;
    description: string;
    parameters: {
      type: "object";
      properties: Record<string, unknown>;
      required: string[];
    };
  };
};

const LLM_MODEL = "gpt-4";
const SYSTEM_PROMPT =
  "You are a financial-data analyst. The host has put a transactions array in your reach. Investigate it and return a structured summary plus any outliers.";
const USER_TASK = "Summarize the latest transaction batch and find outliers.";

const BASH_TOOL: ToolDef = {
  type: "function",
  function: {
    name: "bash",
    description: "Execute a shell command and return its stdout.",
    parameters: {
      type: "object",
      properties: {
        command: { type: "string", description: "The shell command to run." },
      },
      required: ["command"],
    },
  },
};

const RUN_CODE_TOOL: ToolDef = {
  type: "function",
  function: {
    name: "run_code",
    description:
      "Run GocciaScript in a sandbox with the provided globals; returns the script's value plus stdout. The sandbox has no fetch, fs, env, or eval.",
    parameters: {
      type: "object",
      properties: {
        code: {
          type: "string",
          description: "GocciaScript source. Last expression is the result.",
        },
        globals: {
          type: "object",
          description: "Variables injected into the sandbox.",
          additionalProperties: true,
        },
      },
      required: ["code"],
    },
  },
};

const TOOL_CALL_FLOWS: { bash: ToolFlow; goccia: ToolFlow } = {
  bash: {
    label: "Bash + jq",
    argName: "command",
    toolDef: BASH_TOOL,
    steps: [
      {
        tool: "bash",
        call: "ls /tmp/agent/transactions/",
        role: "discover",
        result: "transactions.current.json\n",
      },
      {
        tool: "bash",
        call: "cat /tmp/agent/transactions/transactions.current.json",
        role: "load",
        result:
          '[{"id":1,"amount":42.5},{"id":2,"amount":-12},{"id":3,"amount":98.34},{"id":4,"amount":-7.5},{"id":5,"amount":312},{"id":6,"amount":18.4},{"id":7,"amount":-298.4},{"id":8,"amount":54.2}]\n',
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add' transactions.current.json",
        role: "sum",
        result: "207.54\n",
      },
      {
        tool: "bash",
        call: "jq '[.[].amount] | add / length' transactions.current.json",
        role: "average",
        result: "25.94\n",
      },
      {
        tool: "bash",
        call: "jq '[.[] | select(.amount > 280)]' transactions.current.json",
        role: "outliers",
        result: '[{"id":5,"amount":312},{"id":7,"amount":-298.4}]\n',
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
    toolDef: RUN_CODE_TOOL,
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
        result:
          '{"value":{"total":207.54,"avg":25.94,"outliers":[{"id":5,"amount":312},{"id":7,"amount":-298.4}]},"stdout":""}',
      },
    ],
    risks: [
      "all values stay in one sandbox — no serialization between steps",
      "host gets a single structured JSON result back",
    ],
  },
};

/** Pre-computed tiktoken counts for each turn — split into the input
 *  side (request body the model sees: system + user + tools +
 *  accumulated assistant calls + tool results) and the output side
 *  (the assistant message the model emits). Both are billed; the
 *  output rate is typically 2-3× the input rate, so we surface them
 *  separately in the UI.
 *
 *  Verified once with `gpt-tokenizer`'s `cl100k_base` encoder via
 *  `scripts/compute-llm-call-tokens.mjs`; re-run that script and paste
 *  new arrays if the system prompt, tool defs, step calls, or tool-
 *  result texts change. The tokenizer is intentionally NOT a runtime
 *  dependency (~53 MB unpacked). */
const LLM_CALL_TOKENS = {
  bash: { in: [119, 183, 336, 404, 474], out: [45, 48, 49, 51, 53] },
  goccia: { in: [170], out: [142] },
} as const;

/** Build the full LLM request body the model sees at step `index` of
 *  flow `flowKey`. Includes the system prompt, user task, tool
 *  definitions, and all assistant tool calls + tool results from
 *  prior steps. This is what the LLM is actually billed for on that
 *  turn — the toggle reveals it for direct comparison. */
function buildLlmRequest(
  flowKey: keyof typeof TOOL_CALL_FLOWS,
  index: number,
): unknown {
  const flow = TOOL_CALL_FLOWS[flowKey];
  const messages: unknown[] = [
    { role: "system", content: SYSTEM_PROMPT },
    { role: "user", content: USER_TASK },
  ];
  for (let j = 0; j < index; j++) {
    const prev = flow.steps[j];
    messages.push({
      role: "assistant",
      content: null,
      tool_calls: [
        {
          id: `call_${j + 1}`,
          type: "function",
          function: {
            name: prev.tool,
            arguments: JSON.stringify({ [flow.argName]: prev.call }),
          },
        },
      ],
    });
    messages.push({
      role: "tool",
      tool_call_id: `call_${j + 1}`,
      content: prev.result,
    });
  }
  return { model: LLM_MODEL, messages, tools: [flow.toolDef] };
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
          <span>Show full LLM request body</span>
        </label>
        <span className="tcc-toggle-note">
          Each step expands to the full request the model sees on that turn —
          system prompt, tool definitions, prior assistant calls, and tool
          results. Token counts via{" "}
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
            const flowKey = key as keyof typeof TOOL_CALL_FLOWS;
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
                      {flow.steps.length === 1 ? "" : "s"} ·{" "}
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
                        <pre className="tcc-call">
                          <code>
                            {showPayload ? (
                              <HighlightedGeneric
                                code={JSON.stringify(
                                  buildLlmRequest(flowKey, i),
                                  null,
                                  2,
                                )}
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

/** Default sandbox script — keep the output to ~3 short lines so the
 *  host-result panel doesn't dominate the page layout. The runner
 *  doesn't accept top-level `return` (source treated as a module),
 *  so we use `console.log` for the visible output. */
const DEFAULT_CODE = `// Untrusted script from an AI agent.
// The sandbox has no fetch, fs, env, or eval — by default.
const active = users.filter((u) => u.active);
const names = active.map((u) => u.name.toUpperCase()).join(", ");
console.log("active count:", active.length);
console.log("names:", names);
console.log("first id:", active[0]?.id);`;

const DEFAULT_GLOBALS = `{
  "users": [
    { "id": 1, "name": "Alice",   "active": true  },
    { "id": 2, "name": "Bob",     "active": false },
    { "id": 3, "name": "Charlie", "active": true  }
  ]
}`;

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
                lineNumbers
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
                lineNumbers
              />
            </div>
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
