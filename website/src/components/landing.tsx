"use client";

import { Card } from "@astryxdesign/core/Card";
import { CodeBlock } from "@astryxdesign/core/CodeBlock";
import { Collapsible, CollapsibleGroup } from "@astryxdesign/core/Collapsible";
import { Heading } from "@astryxdesign/core/Heading";
import { Text } from "@astryxdesign/core/Text";
import { VStack } from "@astryxdesign/core/VStack";
import Link from "next/link";
import {
  Fragment,
  type ReactNode,
  useEffect,
  useId,
  useRef,
  useState,
} from "react";
import { AnchorH2, AnchorH3 } from "@/components/anchor-heading";
import { AnimatedOutput } from "@/components/animated-output";
import { useRunShortcut } from "@/components/command-tabs";
import { ConsolePanel } from "@/components/console-panel";
import { HighlightedCode } from "@/components/highlighted-code";
import {
  ArrowIcon,
  BookIcon,
  ClockIcon,
  CopyIcon,
  DropIcon,
  LeafIcon,
  PlayIcon,
  RunIcon,
  ShieldIcon,
} from "@/components/icons";
import { LatestVersion } from "@/components/latest-version";
import { QuickInstall } from "@/components/quick-install";
import type { OutputLine } from "@/lib/examples";
import { formatError } from "@/lib/format-error";
import { formatMemorySegments, type MemoryJson } from "@/lib/format-memory";
import { isPreStable, type ReleaseInfo } from "@/lib/github";
import {
  BUILTINS,
  FEATURES,
  type FeatureIcon,
  PROFILE_DISABLED_FEATURES,
} from "@/lib/landing-data";
import {
  COMPILER_SUPPORT_ANSWER,
  COMPILER_SUPPORT_QUESTION,
  ECMASCRIPT_SCOPE_ANSWER,
  ECMASCRIPT_SCOPE_QUESTION,
  GOCCIASCRIPT_SUMMARY,
  NODE_COMPATIBILITY_ANSWER,
  NODE_COMPATIBILITY_QUESTION,
  TYPE_ANNOTATIONS_ANSWER,
  TYPE_ANNOTATIONS_QUESTION,
} from "@/lib/positioning";

const FEATURE_ICONS: Record<
  FeatureIcon,
  (p: { size?: number }) => React.ReactElement
> = {
  drop: ({ size }) => <DropIcon size={size} />,
  shield: ShieldIcon,
  leaf: LeafIcon,
  clock: ClockIcon,
};

function HeroRunnableCard({ code }: { code: string }) {
  const [copyTick, setCopyTick] = useState(0);
  // Bumped on each new execution so `<AnimatedOutput>` re-mounts and
  // re-runs its line-stagger reveal cleanly. Kept separate from
  // `copyTick` so the "copied" flash on the copy button doesn't replay
  // the console output.
  const [runTick, setRunTick] = useState(0);
  const [running, setRunning] = useState(false);
  const [output, setOutput] = useState<OutputLine[] | null>(null);
  const [src, setSrc] = useState(code);
  const taRef = useRef<HTMLTextAreaElement>(null);
  const hlRef = useRef<HTMLPreElement>(null);
  const runShortcut = useRunShortcut();

  const copy = async () => {
    let ok = false;
    try {
      if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(src);
        ok = true;
      }
    } catch {
      ok = false;
    }
    if (!ok && taRef.current) {
      const ta = taRef.current;
      const prevStart = ta.selectionStart;
      const prevEnd = ta.selectionEnd;
      ta.select();
      try {
        ok = document.execCommand("copy");
      } catch {
        ok = false;
      }
      ta.setSelectionRange(prevStart, prevEnd);
    }
    if (ok) setCopyTick((t) => t + 1);
  };

  useEffect(() => {
    if (copyTick === 0) return;
    const id = setTimeout(() => setCopyTick(0), 1500);
    return () => clearTimeout(id);
  }, [copyTick]);
  const copied = copyTick > 0;

  const run = async () => {
    // The button is `disabled` while running, but the ⌘/Ctrl+Enter
    // shortcut on the textarea bypasses that — guard re-entry directly
    // so a fast double-press can't fire two overlapping `/api/execute`
    // requests and race the output panel.
    if (running) return;
    setRunning(true);
    setRunTick((t) => t + 1);
    const banner = "GocciaScriptLoader coffee-typed.ts";
    setOutput([{ kind: "meta", text: banner }]);
    try {
      const res = await fetch("/api/execute", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ code: src }),
      });
      // Server tags repeat-input responses with `X-Cache: HIT` so we can
      // surface "(cached)" in the exit line. Anything else (MISS, missing,
      // unexpected value) is treated as a fresh run.
      const cached = res.headers.get("X-Cache") === "HIT";
      const data = (await res.json()) as {
        ok?: boolean;
        output?: string;
        error?: {
          type?: string;
          name?: string;
          message: string;
          line?: number | null;
          column?: number | null;
          fileName?: string | null;
        } | null;
        timing?: { total_ms: number };
        memory?: MemoryJson | null;
        exitCode?: number | null;
      };
      const lines: OutputLine[] = [{ kind: "meta", text: banner }];
      if (data.output) {
        for (const line of data.output.replace(/\n$/, "").split("\n")) {
          lines.push({ kind: "out", text: line });
        }
      }
      if (data.error) {
        for (const line of formatError(data.error, src)) {
          lines.push({
            kind: line.kind === "err" ? "err" : "meta",
            text: line.text,
          });
        }
      }
      const totalMs = data.timing?.total_ms;
      lines.push({
        kind: "meta",
        text: `— exit ${data.exitCode ?? "?"}${
          totalMs !== undefined ? ` · ${totalMs.toFixed(2)}ms` : ""
        }${formatMemorySegments(data.memory)}${cached ? " · (cached)" : ""}`,
      });
      // Pad to 5 lines so the hero console keeps a fixed height.
      while (lines.length < 5) {
        lines.push({ kind: "meta", text: "" });
      }
      setOutput(lines);
    } catch (err) {
      // `err` from `fetch` / `res.json()` is *usually* an `Error`
      // subclass (TypeError on network failure, AbortError on
      // disconnect, SyntaxError on bad JSON), but the catch parameter
      // is `unknown` and a misbehaving runtime can throw anything.
      // Normalise rather than blind-cast so the worst case is a
      // `String(err)` fallback rather than rendering `undefined`.
      const message =
        err instanceof Error
          ? err.message
          : typeof err === "string"
            ? err
            : String(err);
      setOutput([
        { kind: "meta", text: banner },
        { kind: "err", text: `network error: ${message}` },
      ]);
    } finally {
      setRunning(false);
    }
  };

  // Run automatically on mount so the hero card shows live output
  // immediately — same UX as the sandbox preview.
  // biome-ignore lint/correctness/useExhaustiveDependencies: run once on mount
  useEffect(() => {
    run();
  }, []);

  // Auto-size the textarea to its content so its internal scroll never
  // engages; the outer .hero-editor handles overflow. With no internal
  // scroll, the overlay (positioned absolute over the same area) and
  // textarea always render at identical y-coordinates, eliminating the
  // bottom-of-content offset between the highlight and the caret.
  // biome-ignore lint/correctness/useExhaustiveDependencies: re-measure on every src change
  useEffect(() => {
    const ta = taRef.current;
    if (!ta) return;
    ta.style.height = "auto";
    ta.style.height = `${ta.scrollHeight}px`;
  }, [src]);

  return (
    <div className="code-card">
      <div className="code-card-head">
        <div className="dots">
          <span />
          <span />
          <span />
        </div>
        <span className="flex-1">coffee-typed.ts</span>
        {copied && (
          <span key={copyTick} className="copied-flash">
            copied
          </span>
        )}
        <button
          type="button"
          className="hero-action"
          onClick={copy}
          title={copied ? "Copied" : "Copy"}
          aria-label="Copy"
        >
          <CopyIcon size={13} />
        </button>
        <Link
          href="/playground?example=coffee-typed"
          className="hero-action"
          title="Open in Playground"
          aria-label="Open in Playground"
        >
          <ArrowIcon size={13} />
        </Link>
        <button
          type="button"
          className="hero-action primary"
          onClick={run}
          disabled={running}
          title={`Run · ${runShortcut.long}`}
          aria-label="Run"
        >
          <RunIcon size={11} />
          <span>{running ? "…" : "Run"}</span>
          <span className="hero-action-kbd" aria-hidden="true">
            {runShortcut.short}
          </span>
        </button>
      </div>
      <div className="hero-editor">
        <div className="hero-editor-inner">
          <pre className="hero-gutter" aria-hidden="true">
            {Array.from({ length: src.split("\n").length }, (_, i) =>
              String(i + 1),
            ).join("\n")}
          </pre>
          <pre className="hero-hl" ref={hlRef} aria-hidden="true">
            <code>
              <HighlightedCode code={`${src}\n`} />
            </code>
          </pre>
          <textarea
            ref={taRef}
            className="hero-ta"
            spellCheck={false}
            value={src}
            onChange={(e) => setSrc(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
                e.preventDefault();
                run();
              }
            }}
          />
        </div>
      </div>
      <ConsolePanel className="hero-console">
        <AnimatedOutput
          runKey={runTick}
          lines={
            output
              ? output.map((l) => ({
                  kind:
                    l.kind === "meta"
                      ? "meta"
                      : l.kind === "err"
                        ? "err"
                        : "out",
                  text: l.text,
                }))
              : []
          }
        />
      </ConsolePanel>
    </div>
  );
}

type ArchStage = {
  id: string;
  label: string;
  small: string;
  overview: ReactNode;
  docId: string;
  docLabel: string;
  accent?: boolean;
};

type AstNode = {
  type: string;
  attrs?: Record<string, string>;
  children?: AstNode[];
};

const AST_TREE: AstNode = {
  type: "Program",
  children: [
    {
      type: "VariableDeclaration",
      attrs: { kind: '"const"', name: '"price"' },
      children: [{ type: "LiteralExpression", attrs: { value: "2.5" } }],
    },
    {
      type: "VariableDeclaration",
      attrs: { kind: '"const"', name: '"qty"' },
      children: [{ type: "LiteralExpression", attrs: { value: "3" } }],
    },
    {
      type: "VariableDeclaration",
      attrs: { kind: '"const"', name: '"total"' },
      children: [
        {
          type: "BinaryExpression",
          attrs: { op: '"*"' },
          children: [
            { type: "IdentifierExpression", attrs: { name: '"price"' } },
            { type: "IdentifierExpression", attrs: { name: '"qty"' } },
          ],
        },
      ],
    },
    {
      type: "ExpressionStatement",
      children: [
        {
          type: "CallExpression",
          children: [
            {
              type: "MemberExpression",
              attrs: { property: '"log"' },
              children: [
                {
                  type: "IdentifierExpression",
                  attrs: { name: '"console"' },
                },
              ],
            },
            { type: "LiteralExpression", attrs: { value: '"total:"' } },
            { type: "IdentifierExpression", attrs: { name: '"total"' } },
          ],
        },
      ],
    },
  ],
};

function AstTreeNode({ node, depth = 0 }: { node: AstNode; depth?: number }) {
  const [open, setOpen] = useState(true);
  const hasChildren = !!node.children && node.children.length > 0;
  return (
    <li className="ast-node">
      <div className="ast-row" style={{ paddingLeft: depth * 14 }}>
        {hasChildren ? (
          <button
            type="button"
            className="ast-toggle"
            aria-expanded={open}
            aria-label={open ? "Collapse" : "Expand"}
            onClick={() => setOpen(!open)}
          >
            {open ? "▾" : "▸"}
          </button>
        ) : (
          <span className="ast-bullet" aria-hidden="true">
            ·
          </span>
        )}
        <span className="ast-type">{node.type}</span>
        {node.attrs &&
          Object.entries(node.attrs).map(([k, v]) => (
            <span key={k} className="ast-attr">
              <span className="ast-attr-key">{k}</span>
              <span className="ast-attr-eq">=</span>
              <span className="ast-attr-val">{v}</span>
            </span>
          ))}
      </div>
      {hasChildren && open && (
        <ul className="ast-children">
          {node.children?.map((c, i) => (
            <AstTreeNode key={i} node={c} depth={depth + 1} />
          ))}
        </ul>
      )}
    </li>
  );
}

function ArchDiagram() {
  const SOURCE = `const price = 2.5;
const qty = 3;
const total = price * qty;
console.log("total:", total);`;

  const TOKENS: { t: string; v: string }[] = [
    { t: "keyword", v: "const" },
    { t: "identifier", v: "price" },
    { t: "operator", v: "=" },
    { t: "number", v: "2.5" },
    { t: "punctuation", v: ";" },
    { t: "newline", v: "⏎" },
    { t: "keyword", v: "const" },
    { t: "identifier", v: "qty" },
    { t: "operator", v: "=" },
    { t: "number", v: "3" },
    { t: "punctuation", v: ";" },
    { t: "newline", v: "⏎" },
    { t: "keyword", v: "const" },
    { t: "identifier", v: "total" },
    { t: "operator", v: "=" },
    { t: "identifier", v: "price" },
    { t: "operator", v: "*" },
    { t: "identifier", v: "qty" },
    { t: "punctuation", v: ";" },
    { t: "newline", v: "⏎" },
    { t: "identifier", v: "console" },
    { t: "punctuation", v: "." },
    { t: "identifier", v: "log" },
    { t: "punctuation", v: "(" },
    { t: "string", v: '"total:"' },
    { t: "punctuation", v: "," },
    { t: "identifier", v: "total" },
    { t: "punctuation", v: ")" },
    { t: "punctuation", v: ";" },
  ];

  const BYTECODE = [
    { op: "OP_LOAD_CONST", arg: "r0, k0", note: "k0 = 2.5" },
    { op: "OP_SET_LOCAL", arg: "price, r0", note: "price = 2.5" },
    { op: "OP_LOAD_INT", arg: "r1, 3", note: "small int inline" },
    { op: "OP_SET_LOCAL", arg: "qty, r1", note: "qty = 3" },
    { op: "OP_GET_LOCAL", arg: "r2, price", note: "" },
    { op: "OP_GET_LOCAL", arg: "r3, qty", note: "" },
    { op: "OP_MUL", arg: "r2, r2, r3", note: "2.5 × 3 = 7.5" },
    { op: "OP_SET_LOCAL", arg: "total, r2", note: "total = 7.5" },
    { op: "OP_GET_GLOBAL", arg: "r4, console", note: "" },
    { op: "OP_GET_PROP_CONST", arg: "r5, r4, log", note: "console.log" },
    { op: "OP_LOAD_CONST", arg: "r6, k1", note: 'k1 = "total:"' },
    { op: "OP_GET_LOCAL", arg: "r7, total", note: "" },
    { op: "OP_CALL_METHOD", arg: "r5, r4, 2", note: "console.log(...)" },
    { op: "OP_RETURN", arg: "", note: "" },
  ];

  const RESULT = `{
  "result": null,
  "stdout": "total: 7.5\\n",
  "fileName": "example.js",
  "lexTimeNanoseconds": 14800,
  "parseTimeNanoseconds": 41600,
  "compileTimeNanoseconds": 23200,
  "executeTimeNanoseconds": 5300,
  "totalTimeNanoseconds": 84900
}`;

  const CONSOLE_OUTPUT = `total: 7.5`;

  const stages: ArchStage[] = [
    {
      id: "source",
      label: "Source",
      small: ".js · .ts · .jsx · .tsx · .mjs",
      overview:
        "Every supported extension enters the same source pipeline. GocciaScript implements the TC39 Type Annotations proposal, so supported annotations have no runtime effect by default; --strict-types optionally adds runtime contracts. JSX is rewritten to function calls in a preprocessing pass before lexing.",
      docId: "language",
      docLabel: "Language",
    },
    {
      id: "lexer",
      label: "Lexer",
      small: "tokens",
      overview:
        "A hand-written scanner streams the source into typed tokens with line and column positions. Newlines are emitted as their own tokens — ASI is the parser's job, not the lexer's, so error spans always point at real characters.",
      docId: "architecture",
      docLabel: "Architecture",
    },
    {
      id: "parser",
      label: "Parser",
      small: "AST",
      overview: (
        <>
          A recursive-descent parser with precedence climbing for binary
          operators. Recommended defaults reject legacy or coercive constructs (
          <code className={inlineCodeClass}>var</code>,{" "}
          <code className={inlineCodeClass}>==</code>,{" "}
          <code className={inlineCodeClass}>with</code>) unless their explicit
          compatibility flags are enabled. Normal runtimes do not install{" "}
          <code className={inlineCodeClass}>eval</code>.
        </>
      ),
      docId: "language",
      docLabel: "Language reference",
    },
    {
      id: "compiler",
      label: "Compiler & VM",
      small: "register bytecode → .gbc",
      overview:
        "Default execution is a tree-walk evaluator over the AST. With --mode=bytecode the compiler lowers the AST to a register-based bytecode and the VM executes that — same object model, same scope chain, just a different backend.",
      docId: "bytecode-vm",
      docLabel: "Bytecode VM",
    },
    {
      id: "result",
      label: "Result",
      small: "value · timings",
      overview:
        "Every run returns a structured result: the final value plus a per-stage timing breakdown (lex, parse, compile, execute, total) in nanoseconds. Console output is captured separately. Hosts get a single object — no streams to plumb.",
      docId: "architecture",
      docLabel: "Architecture",
    },
  ];

  const [active, setActive] = useState(0);
  const [resultView, setResultView] = useState<"console" | "json">("console");
  const stage = stages[active];

  const tabConsoleId = useId();
  const tabJsonId = useId();
  const panelConsoleId = useId();
  const panelJsonId = useId();

  return (
    <div className="arch">
      <div className="arch-row">
        {stages.map((s, i) => (
          <Fragment key={s.id}>
            <button
              type="button"
              className={`arch-node${active === i ? " is-active" : ""}${s.accent ? " is-accent" : ""}`}
              onClick={() => setActive(i)}
              aria-pressed={active === i}
            >
              <span className="arch-num">{i + 1}</span>
              <span className="arch-label">{s.label}</span>
              <span className="arch-small">{s.small}</span>
            </button>
            {i < stages.length - 1 && (
              <div className="arch-arrow" aria-hidden="true">
                <ArrowIcon size={18} />
              </div>
            )}
          </Fragment>
        ))}
      </div>

      <div className="arch-detail">
        <div className="arch-detail-head">
          <span className="arch-detail-step">
            Step {active + 1} of {stages.length}
          </span>
          <h4>{stage.label}</h4>
        </div>
        <p key={`overview-${active}`} className="arch-detail-anim">
          {stage.overview}
        </p>

        <div
          key={`artifact-${active}`}
          className="arch-artifact arch-detail-anim"
        >
          {active === 0 && (
            <div className="arch-artifact-box">
              <div className="arch-artifact-head">example.js</div>
              <CodeBlock
                code={SOURCE}
                language="javascript"
                hasLanguageLabel={false}
                hasLineNumbers
                hasCopyButton={false}
                size="sm"
                width="100%"
              />
            </div>
          )}

          {active === 1 && (
            <div className="arch-artifact-box">
              <div className="arch-artifact-head">
                token stream · {TOKENS.length} tokens
              </div>
              <div className="arch-tokens">
                {TOKENS.map((tok, i) => (
                  <span
                    key={i}
                    className={`arch-tok tok-${tok.t}`}
                    title={tok.t}
                  >
                    <span className="arch-tok-kind">{tok.t}</span>
                    <span className="arch-tok-val">{tok.v}</span>
                  </span>
                ))}
              </div>
            </div>
          )}

          {active === 2 && (
            <div className="arch-artifact-box">
              <div className="arch-artifact-head">abstract syntax tree</div>
              <ul className="arch-ast-tree">
                <AstTreeNode node={AST_TREE} />
              </ul>
            </div>
          )}

          {active === 3 && (
            <div className="arch-artifact-box">
              <div className="arch-artifact-head">
                bytecode · {BYTECODE.length} ops
              </div>
              <div className="arch-bc">
                {BYTECODE.map((b, i) => (
                  <div key={i} className="arch-bc-row">
                    <span className="arch-bc-addr">
                      {String(i).padStart(2, "0")}
                    </span>
                    <span className="arch-bc-op">{b.op}</span>
                    <span className="arch-bc-arg">{b.arg}</span>
                    {b.note && <span className="arch-bc-note">; {b.note}</span>}
                  </div>
                ))}
              </div>
            </div>
          )}

          {active === 4 && (
            <div className="arch-artifact-box">
              <div
                className="arch-artifact-head arch-artifact-tabs"
                role="tablist"
              >
                <button
                  type="button"
                  role="tab"
                  id={tabConsoleId}
                  aria-selected={resultView === "console"}
                  aria-controls={panelConsoleId}
                  className="arch-artifact-tab"
                  data-active={resultView === "console"}
                  onClick={() => setResultView("console")}
                >
                  Console output
                </button>
                <button
                  type="button"
                  role="tab"
                  id={tabJsonId}
                  aria-selected={resultView === "json"}
                  aria-controls={panelJsonId}
                  className="arch-artifact-tab"
                  data-active={resultView === "json"}
                  onClick={() => setResultView("json")}
                >
                  JSON result
                </button>
              </div>
              {resultView === "console" ? (
                <div
                  role="tabpanel"
                  id={panelConsoleId}
                  aria-labelledby={tabConsoleId}
                >
                  <ConsolePanel className="arch-console">
                    <AnimatedOutput
                      runKey={`console-${active}`}
                      showCaret={false}
                      lines={[
                        {
                          kind: "meta" as const,
                          text: "GocciaScriptLoader example.js",
                        },
                        ...CONSOLE_OUTPUT.split("\n").map((text) => ({
                          kind: "out" as const,
                          text,
                        })),
                        {
                          kind: "meta" as const,
                          text: "— exit 0 · 0.08ms",
                        },
                      ]}
                    />
                  </ConsolePanel>
                </div>
              ) : (
                <div
                  role="tabpanel"
                  id={panelJsonId}
                  aria-labelledby={tabJsonId}
                >
                  <CodeBlock
                    className="arch-detail-anim"
                    code={RESULT}
                    language="json"
                    hasLanguageLabel={false}
                    hasLineNumbers
                    hasCopyButton={false}
                    size="sm"
                    width="100%"
                  />
                </div>
              )}
            </div>
          )}
        </div>

        <Link href={`/docs/${stage.docId}`} className="arch-detail-link">
          Read more in <strong>{stage.docLabel}</strong> <ArrowIcon size={14} />
        </Link>
      </div>
    </div>
  );
}

const HERO_CODE = `// Types, enums, private fields.
type Currency = "USD" | "EUR" | "GBP";

enum DrinkSize {
  Small = "small",
  Medium = "medium",
  Large = "large",
}

interface MenuItem {
  name: string;
  basePrice: number;
}

class CoffeeShop {
  #name: string = "Goccia Coffee";
  #menu: MenuItem[] = [
    { name: "Espresso", basePrice: 2.5 },
    { name: "Latte", basePrice: 4.0 },
  ];

  total(
    items: string[],
    size: DrinkSize,
    currency: Currency,
  ): string {
    const m =
      size === DrinkSize.Large ? 1.3 :
      size === DrinkSize.Medium ? 1.1 : 1.0;
    const sub = items.reduce((s, it) => {
      const f = this.#menu
        .find((x) => x.name === it);
      return s + (f ? f.basePrice * m : 0);
    }, 0);
    const sym: Record<Currency, string> = {
      USD: "$", EUR: "€", GBP: "£",
    };
    return \`\${sym[currency]}\${sub.toFixed(2)}\`;
  }

  get name(): string {
    return this.#name;
  }
}

const shop = new CoffeeShop();
const total = shop.total(
  ["Espresso", "Latte"],
  DrinkSize.Medium,
  "EUR",
);
console.log(\`Welcome to \${shop.name}!\`);
console.log(\`Your total: \${total}\`);`;

const inlineCodeClass = "font-mono bg-paper-2 px-[0.3em] py-[0.05em] rounded";

const FAQ_ITEMS: { question: string; answer: ReactNode }[] = [
  {
    question: "What is GocciaScript?",
    answer: GOCCIASCRIPT_SUMMARY,
  },
  {
    question: "What is GocciaScript for?",
    answer:
      "Its primary goal is JavaScript execution for AI agents under an explicit, host-defined capability model. The same runtime and toolchain also serve sandboxed automation, portable native applications, plugins, and desktop scripting; native embedding is an important secondary goal.",
  },
  {
    question: ECMASCRIPT_SCOPE_QUESTION,
    answer: ECMASCRIPT_SCOPE_ANSWER,
  },
  {
    question: TYPE_ANNOTATIONS_QUESTION,
    answer: TYPE_ANNOTATIONS_ANSWER,
  },
  {
    question: NODE_COMPATIBILITY_QUESTION,
    answer: NODE_COMPATIBILITY_ANSWER,
  },
  {
    question: "Is GocciaScript a browser runtime?",
    answer:
      "No. It implements selected web-standard APIs where they fit the sandboxed runtime and embeddable-platform goals, but it does not provide a DOM, Web Workers, storage APIs, or a full browser event loop.",
  },
  {
    question: COMPILER_SUPPORT_QUESTION,
    answer: COMPILER_SUPPORT_ANSWER,
  },
  {
    question: "Is GocciaScript production-ready?",
    answer: (
      <>
        GocciaScript is still pre-1.0, so APIs and compatibility behavior can
        change between releases. Its current{" "}
        <Link href="/compatibility" className="link-button">
          test262 results
        </Link>{" "}
        already makes it suitable for serious experiments and bounded production
        use where the supported surface matches your needs.
      </>
    ),
  },
  {
    question: "How does the sandbox model work?",
    answer:
      "Scripts have no ambient host filesystem, process, native FFI, or network authority by default. GocciaSandboxRunner imports explicit seed baselines into a virtual filesystem, exposes fs/goccia as import-only modules, and reports sandbox writes as diffs instead of writing back to host paths.",
  },
  {
    question: "How compatible is it with ECMAScript?",
    answer: (
      <>
        GocciaScript tracks ECMAScript compatibility through generated test262
        reports. The{" "}
        <Link href="/compatibility" className="link-button">
          compatibility dashboard
        </Link>{" "}
        shows the current main-branch result set.
      </>
    ),
  },
  {
    question: "Can GocciaScript run TypeScript or JSX source?",
    answer: TYPE_ANNOTATIONS_ANSWER,
  },
  {
    question: "How do I try GocciaScript?",
    answer: (
      <>
        Use the{" "}
        <Link href="/playground?example=coffee-typed" className="link-button">
          playground
        </Link>{" "}
        in the browser, or install the command-line tools from the{" "}
        <Link href="/installation" className="link-button">
          installation guide
        </Link>
        .
      </>
    ),
  },
];

export function Landing({
  compatibility,
  release,
  locale,
}: {
  compatibility?: {
    passed: number;
    totalRun: number;
  } | null;
  release?: ReleaseInfo | null;
  /** BCP-47 locale tag from `Accept-Language` so the release-date in
   *  the hero "Latest version" block formats the same on SSR and in
   *  the browser. */
  locale: string;
}) {
  return (
    <>
      <section className="hero">
        <div className="container">
          <div className="hero-grid">
            <div>
              <h1>
                A <span className="drop">drop</span> of
                <br />
                Java<span className="script">Script</span>, sandboxed.
              </h1>
              <p className="hero-lede">
                A sandbox-first{" "}
                <a
                  href="https://tc39.es/ecma262/"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="info-term"
                  aria-describedby="ecmascript-tip"
                >
                  ECMAScript
                  <span
                    id="ecmascript-tip"
                    role="tooltip"
                    className="info-tooltip"
                  >
                    ECMAScript is the language specified by ECMA-262.
                    GocciaScript uses modern recommended defaults while tracking
                    conformance with generated test262 reports.
                  </span>
                </a>{" "}
                runtime and toolchain for AI agents. The host defines the
                available capabilities, runtime surface, and execution limits.
              </p>
              <div className="hero-cta-row">
                <Link
                  href="/playground?example=coffee-typed"
                  className="btn btn-primary"
                >
                  <PlayIcon size={16} /> Open the Playground
                </Link>
                <Link href="/docs" className="btn btn-ghost">
                  <BookIcon size={16} /> Read the docs
                </Link>
              </div>
              <div className="mt-5 hero-quick-install">
                <QuickInstall />
                <div className="hero-install-meta">
                  <LatestVersion release={release ?? null} locale={locale} />
                  {/* `/install` is an internal Next.js route — use
                      `<Link>` for soft navigation + prefetch. We keep
                      `target="_blank"` because the UX is "peek at the
                      script in a new tab without losing the landing
                      page state". */}
                  <Link
                    href="/install"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="link-button text-[0.85rem] text-ink-3"
                  >
                    View install script
                  </Link>
                  <Link
                    href="/installation"
                    className="link-button text-[0.85rem] text-ink-3"
                  >
                    Other install options →
                  </Link>
                </div>
              </div>
              {isPreStable(release) && (
                <p className="prestable-note mt-3">
                  <strong>Pre-1.0:</strong> the API is still in motion — expect
                  breaking changes between any two releases until 1.0.
                </p>
              )}
            </div>
            <div className="hero-visual">
              <HeroRunnableCard code={HERO_CODE} />
            </div>
          </div>
        </div>
      </section>

      <section className="content-section">
        <div className="container">
          <div className="section-head">
            <div className="section-kicker">ECMAScript compatibility</div>
            <AnchorH2 id="design-principles" className="hd-no-wrap">
              The implementation is broader than{" "}
              <span className="wave-under">the recommended profile.</span>
            </AnchorH2>
            <p>
              GocciaScript implements core ECMAScript. Modern, explicit forms
              are enabled by default; every standard core form disabled by the
              recommended profile has an explicit compatibility path. Normal
              hosts do not install <code className={inlineCodeClass}>eval</code>
              , which is exposed only by the private test262 conformance host.
            </p>
            <p className="compat-note">
              {compatibility ? (
                <>
                  Current generated evidence:{" "}
                  <strong>
                    {compatibility.passed.toLocaleString(locale)} of{" "}
                    {compatibility.totalRun.toLocaleString(locale)}
                  </strong>{" "}
                  cases pass in the latest published non-Annex-B test262 run.
                </>
              ) : (
                <>Current generated test262 evidence is published separately.</>
              )}{" "}
              <Link href="/compatibility" className="link-button">
                Open the live compatibility dashboard
              </Link>
              .
            </p>
          </div>
          <div className="feature-grid">
            {FEATURES.map((f) => {
              const I = FEATURE_ICONS[f.icon];
              return (
                <Card
                  key={f.title}
                  className="paper-card feature-card"
                  padding={6}
                >
                  <VStack gap={4}>
                    <div className="ficon">
                      <I size={20} />
                    </div>
                    <VStack gap={1}>
                      <Heading level={4}>{f.title}</Heading>
                      <Text as="p" type="supporting" display="block">
                        {f.body}
                      </Text>
                    </VStack>
                  </VStack>
                </Card>
              );
            })}
          </div>
        </div>
      </section>

      <section className="content-section pt-0">
        <div className="container">
          <div className="grid grid-cols-[repeat(auto-fit,minmax(300px,1fr))] gap-8">
            <div>
              <div className="section-kicker mb-3">
                Recommended language profile
              </div>
              <AnchorH3 id="recommended-profile" className="mb-3">
                Implemented, with explicit defaults
              </AnchorH3>
              <p className="text-ink-2 mb-4">
                The profile is host policy, not a list of missing language
                features. The table distinguishes implemented semantics from
                their default exposure. See{" "}
                <Link href="/docs/language" className="link-button">
                  Language
                </Link>{" "}
                for the per-feature rationale.
              </p>
              <section
                className="profile-table-wrap"
                aria-label="Recommended profile compatibility paths"
              >
                <table className="profile-table">
                  <thead>
                    <tr>
                      <th scope="col">Form</th>
                      <th scope="col">Implemented</th>
                      <th scope="col">Default</th>
                      <th scope="col">Enablement / exposure</th>
                      <th scope="col">Source</th>
                    </tr>
                  </thead>
                  <tbody>
                    {PROFILE_DISABLED_FEATURES.map((feature) => (
                      <tr key={feature.name}>
                        <th scope="row" title={feature.why}>
                          {feature.name}
                        </th>
                        <td>{feature.implemented ? "Yes" : "No"}</td>
                        <td>{feature.defaultProfile}</td>
                        <td>
                          <code>{feature.enablement}</code>
                        </td>
                        <td>{feature.standardsSource}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </section>
              <p className="compat-note">
                Compatibility flags primarily serve ECMAScript conformance and
                existing code. Dynamic source exposure is separate: normal hosts
                do not install <code className={inlineCodeClass}>eval</code>,
                and <code className={inlineCodeClass}>Function()</code> requires
                the explicit{" "}
                <code className={inlineCodeClass}>
                  --unsafe-function-constructor
                </code>{" "}
                opt-in.
              </p>
            </div>
            <div>
              <div className="section-kicker mb-3">Runtime surface</div>
              <AnchorH3 id="runtime-globals" className="mb-3">
                Beyond the language: runtime and sandbox APIs
              </AnchorH3>
              <p className="text-ink-2 mb-4">
                A broad ECMAScript surface, plus first-class structured-data
                formats — direct module imports of{" "}
                <code className={inlineCodeClass}>.json</code>,{" "}
                <code className={inlineCodeClass}>.json5</code>,{" "}
                <code className={inlineCodeClass}>.toml</code>,{" "}
                <code className={inlineCodeClass}>.yaml</code>,{" "}
                <code className={inlineCodeClass}>.csv</code>,{" "}
                <code className={inlineCodeClass}>.tsv</code>,{" "}
                <code className={inlineCodeClass}>.jsonl</code>, and{" "}
                <code className={inlineCodeClass}>.md</code>; runtime parsers
                for each (markdown imports return the raw text); import maps;
                console output; SemVer helpers; and a built-in{" "}
                <Link href="/docs/testing" className="link-button">
                  test runner
                </Link>{" "}
                with a Vitest/Jest-compatible{" "}
                <code className={inlineCodeClass}>test</code>/
                <code className={inlineCodeClass}>describe</code>/
                <code className={inlineCodeClass}>expect</code> API. In
                GocciaSandboxRunner, the sandbox surface adds import-only{" "}
                <code className={inlineCodeClass}>fs</code> and{" "}
                <code className={inlineCodeClass}>goccia</code> modules backed
                by a seeded virtual filesystem, sandbox shell commands, nested
                execution, and explicit diffs.
              </p>
              <p className="text-ink-2 mb-4">
                The sandbox <code className={inlineCodeClass}>fs</code> module
                provides Node-compatible synchronous, callback, and promise
                methods over that virtual filesystem, including Stats objects
                and Node-shaped errors. GocciaScript is not a complete Node.js
                host: it does not provide CommonJS, npm package resolution,{" "}
                <code className={inlineCodeClass}>process</code>,{" "}
                <code className={inlineCodeClass}>Buffer</code>, or the general{" "}
                <code className={inlineCodeClass}>node:</code> module set.
              </p>
              <div className="builtins-grid">
                {BUILTINS.map((b) => {
                  const slug = b.name.replace(/[^A-Za-z0-9]+/g, "-");
                  return (
                    <button
                      key={b.name}
                      type="button"
                      className="bi-chip"
                      aria-describedby={`bi-tip-${slug}`}
                    >
                      {b.name}
                      <span
                        id={`bi-tip-${slug}`}
                        role="tooltip"
                        className="bi-tooltip"
                      >
                        <code>
                          <HighlightedCode code={b.snippet} />
                        </code>
                      </span>
                    </button>
                  );
                })}
              </div>
            </div>
          </div>
        </div>
      </section>

      <section className="content-section pt-0">
        <div className="container">
          <div className="section-head">
            <div className="section-kicker">Native platform</div>
            <AnchorH2 id="native-platform">
              Portable by design,{" "}
              <span className="wave-under">embeddable by choice.</span>
            </AnchorH2>
            <p>
              FreePascal is the cross-platform toolchain. Delphi support covers
              the complete shipped Win32 and Win64 application matrix under the
              same runtime semantics. Native application embedding remains an
              important secondary goal.
            </p>
          </div>
          <div className="feature-grid">
            <Card className="paper-card feature-card" padding={6}>
              <VStack gap={2}>
                <Heading level={4}>FreePascal</Heading>
                <Text as="p" type="supporting" display="block">
                  Builds the cross-platform command-line toolchain and provides
                  the documented native embedding path.
                </Text>
              </VStack>
            </Card>
            <Card className="paper-card feature-card" padding={6}>
              <VStack gap={2}>
                <Heading level={4}>Delphi 12</Heading>
                <Text as="p" type="supporting" display="block">
                  Repository projects cover every shipped application on Win32
                  and Win64; the support contract includes all applicable Pascal
                  and JavaScript tests.
                </Text>
              </VStack>
            </Card>
            <Card className="paper-card feature-card" padding={6}>
              <VStack gap={2}>
                <Heading level={4}>Native embedding</Heading>
                <Text as="p" type="supporting" display="block">
                  Hosts can embed portable JavaScript and define application
                  globals, modules, capabilities, limits, and result handling.
                </Text>
              </VStack>
            </Card>
          </div>
        </div>
      </section>

      <section className="content-section pt-0">
        <div className="container">
          <div className="section-head">
            <div className="section-kicker">FAQ</div>
            <AnchorH2 id="faq">GocciaScript questions, answered.</AnchorH2>
            <p>
              Short answers to the questions people usually ask before trying or
              integrating the runtime.
            </p>
          </div>
          <Card className="paper-card faq-list" padding={0}>
            <CollapsibleGroup type="multiple" hasDividers density="balanced">
              {FAQ_ITEMS.map((item) => (
                <Collapsible
                  key={item.question}
                  trigger={item.question}
                  value={item.question}
                >
                  <Text
                    as="div"
                    type="large"
                    color="secondary"
                    weight="normal"
                    display="block"
                  >
                    {item.answer}
                  </Text>
                </Collapsible>
              ))}
            </CollapsibleGroup>
          </Card>
        </div>
      </section>

      <section className="content-section border-y border-rule-soft bg-[color-mix(in_oklab,var(--paper-2)_65%,transparent)]">
        <div className="container">
          <div className="section-head">
            <div className="section-kicker">Compiler pipeline</div>
            <AnchorH2 id="compiler-pipeline">From source to result.</AnchorH2>
            <p>
              One pipeline, five stages. Click a stage to jump into the matching
              docs chapter — start with{" "}
              <Link href="/docs/architecture" className="link-button">
                Architecture
              </Link>
              .
            </p>
          </div>
          <div className="arch-card rounded-card border border-rule-soft bg-paper-2 p-8 shadow-paper-sm">
            <ArchDiagram />
          </div>
        </div>
      </section>
    </>
  );
}
