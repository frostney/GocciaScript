"use client";

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
import { ConsolePanel } from "@/components/console-panel";
import {
  HighlightedCode,
  HighlightedGeneric,
} from "@/components/highlighted-code";
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
import { NumberedCode } from "@/components/numbered-code";
import { QuickInstall } from "@/components/quick-install";
import type { OutputLine } from "@/lib/examples";
import { formatError } from "@/lib/format-error";
import { isPreStable, type ReleaseInfo } from "@/lib/github";
import {
  BUILTINS,
  EXCLUDED,
  FEATURES,
  type FeatureIcon,
} from "@/lib/landing-data";

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
    // so a fast double-press can't fire two overlapping `/api/run`
    // requests and race the output panel.
    if (running) return;
    setRunning(true);
    setRunTick((t) => t + 1);
    const banner = "GocciaScriptLoader coffee-shop.js";
    setOutput([{ kind: "meta", text: banner }]);
    try {
      const res = await fetch("/api/run", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ code: src }),
      });
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
        }`,
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
        <span className="flex-1">coffee-shop.js</span>
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
          title="Run"
          aria-label="Run"
        >
          <RunIcon size={11} /> <span>{running ? "…" : "Run"}</span>
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
        "Every supported extension is the same language — TS/TSX type annotations are parsed and discarded, and JSX is rewritten to function calls in a preprocessing pass before the lexer ever sees it. There is no separate type-checker.",
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
          operators. Excluded constructs (
          <code className={inlineCodeClass}>var</code>,{" "}
          <code className={inlineCodeClass}>==</code>,{" "}
          <code className={inlineCodeClass}>eval</code>,{" "}
          <code className={inlineCodeClass}>with</code>) are rejected at parse
          time, so the runtime never has to think about them.
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
              <NumberedCode lineCount={SOURCE.split("\n").length}>
                <HighlightedCode code={SOURCE} />
              </NumberedCode>
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
                  <NumberedCode
                    className="arch-detail-anim"
                    lineCount={RESULT.split("\n").length}
                  >
                    <HighlightedGeneric code={RESULT} language="json" />
                  </NumberedCode>
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

export function Landing({
  release,
  locale,
}: {
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
                Java<span className="script">Script</span>.
              </h1>
              <p className="hero-lede">
                A strict subset of ECMAScript 2027+, implemented from scratch —
                with a sandbox-first runtime designed for tinkerers, embedding
                and AI agents.
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

      <section className="section">
        <div className="container">
          <div className="section-head">
            <div className="section-kicker">Design principles</div>
            <AnchorH2 id="design-principles" className="hd-no-wrap">
              Modern JavaScript,{" "}
              <span className="wave-under">without the quirks.</span>
            </AnchorH2>
            <p>
              What remains is a small, predictable language with a portable
              runtime under 5MB.
            </p>
          </div>
          <div className="feature-grid">
            {FEATURES.map((f) => {
              const I = FEATURE_ICONS[f.icon];
              return (
                <div key={f.title} className="feature">
                  <div className="ficon">
                    <I size={20} />
                  </div>
                  <h4>{f.title}</h4>
                  <p>{f.body}</p>
                </div>
              );
            })}
          </div>
        </div>
      </section>

      <section className="section pt-0">
        <div className="container">
          <div className="grid grid-cols-[repeat(auto-fit,minmax(300px,1fr))] gap-8">
            <div>
              <div className="section-kicker mb-3">Intentionally excluded</div>
              <AnchorH3 id="excluded" className="mb-3">
                What&apos;s left out — and why
              </AnchorH3>
              <p className="text-ink-2 mb-4">
                Some constructs are excluded because they&apos;re security risks{" "}
                (<code className={inlineCodeClass}>eval</code>,{" "}
                <code className={inlineCodeClass}>Function()</code>); some have
                ambiguous semantics (<code className={inlineCodeClass}>==</code>
                , <code className={inlineCodeClass}>var</code>); others have a
                clearer alternative already in the language. See{" "}
                <Link href="/docs/language" className="link-button">
                  Language
                </Link>{" "}
                for the per-feature rationale.
              </p>
              <div className="excluded-row">
                {EXCLUDED.map((x, i) => {
                  // Append the index because the alphanumeric-only slug
                  // would otherwise collide on operator-only names —
                  // `"=="` and `"!="` both reduce to `"-"`, which would
                  // duplicate `id="ex-tip--"` and break the
                  // `aria-describedby` link for whichever pair lost the
                  // DOM lookup. The index keeps the id stable per row
                  // without relying on the actual operator characters.
                  const slug = `${x.name.replace(/[^A-Za-z0-9]+/g, "-")}-${i}`;
                  const tooltipId = `ex-tip-${slug}`;
                  return (
                    <button
                      key={x.name}
                      type="button"
                      className="excluded-pill"
                      aria-describedby={tooltipId}
                    >
                      {x.name}
                      <span
                        id={tooltipId}
                        role="tooltip"
                        className="bi-tooltip"
                      >
                        <code>
                          <HighlightedCode code={x.snippet} />
                        </code>
                      </span>
                    </button>
                  );
                })}
              </div>
              <div className="mt-10">
                <p className="text-ink-2 mb-0 text-[0.92rem]">
                  The runtime is also intentionally{" "}
                  <strong>not Node.js-compatible</strong> — no{" "}
                  <code className={inlineCodeClass}>process</code>, no{" "}
                  <code className={inlineCodeClass}>Buffer</code>, no CommonJS,
                  no ambient filesystem globals.
                </p>
              </div>
            </div>
            <div>
              <div className="section-kicker mb-3">Runtime globals</div>
              <AnchorH3 id="runtime-globals" className="mb-3">
                Beyond the language: a runtime standard library
              </AnchorH3>
              <p className="text-ink-2 mb-4">
                Everything ECMAScript gives you, plus first-class
                structured-data formats — direct module imports of{" "}
                <code className={inlineCodeClass}>.json</code>,{" "}
                <code className={inlineCodeClass}>.json5</code>,{" "}
                <code className={inlineCodeClass}>.toml</code>,{" "}
                <code className={inlineCodeClass}>.yaml</code>,{" "}
                <code className={inlineCodeClass}>.csv</code>,{" "}
                <code className={inlineCodeClass}>.tsv</code>,{" "}
                <code className={inlineCodeClass}>.jsonl</code>, and{" "}
                <code className={inlineCodeClass}>.md</code>; runtime parsers
                for each (markdown imports return the raw text); a
                capability-gated <code className={inlineCodeClass}>fetch</code>{" "}
                (GET/HEAD only, explicit allow-listed hosts); Temporal for
                unambiguous date math; and a built-in{" "}
                <Link href="/docs/testing" className="link-button">
                  test runner
                </Link>{" "}
                with a Vitest/Jest-compatible{" "}
                <code className={inlineCodeClass}>test</code>/
                <code className={inlineCodeClass}>describe</code>/
                <code className={inlineCodeClass}>expect</code> API.
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

      <section className="section border-y border-rule-soft bg-[color-mix(in_oklab,var(--paper-2)_65%,transparent)]">
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
          <div className="rounded-card border border-rule-soft bg-paper-2 p-8 shadow-paper-sm">
            <ArchDiagram />
          </div>
        </div>
      </section>
    </>
  );
}
