export type Builtin = { name: string; snippet: string };

export const BUILTINS: Builtin[] = [
  {
    name: "JSON5",
    snippet: `import cfg from "./goccia.json5";
const cfg2 = JSON5.parse(text);
// trailing commas, comments, unquoted keys`,
  },
  {
    name: "TOML",
    snippet: `import cfg from "./pkg.toml";
const t = TOML.parse(text);
// t.server.port, t.features.sandbox`,
  },
  {
    name: "YAML",
    snippet: `import data from "./manifest.yaml";
const y = YAML.parse(text);
// flow + block, anchors, multi-doc`,
  },
  {
    name: "JSONL",
    snippet: `import rows from "./events.jsonl";
const list = JSONL.parse(text);
// → array of objects, one per line`,
  },
  {
    name: "CSV",
    snippet: `const rows = CSV.parse(text, {
  header: true,
});  // → [{ name, price }, …]`,
  },
  {
    name: "TSV",
    snippet: `const rows = TSV.parse(text, {
  header: true,
});  // tab-separated, same shape`,
  },
  {
    name: ".md",
    snippet: `import readme from "./README.md";
// → raw markdown text (string)
console.log(readme.length);`,
  },
  {
    name: "fetch",
    snippet: `const res = await fetch(url, {
  headers: { Accept: "application/json" },
});  // GET / HEAD only, opt-in hosts`,
  },
  {
    name: "console",
    snippet: `console.log("total:", 6.5);
console.warn("deprecated");
console.error("oops");`,
  },
  {
    name: "Temporal",
    snippet: `const d = Temporal.PlainDate
  .from("2026-05-01")
  .add({ days: 30 });`,
  },
  {
    name: "test / expect",
    snippet: `// GocciaTestRunner — Vitest/Jest-compatible API.
test("adds", () => { expect(1 + 1).toBe(2); });
describe("group", () => { test("…", () => {}); });`,
  },
];

export type Excluded = {
  name: string;
  /** Why excluded (one short phrase). */
  why: string;
  /** Code snippet showing the GocciaScript alternative. */
  snippet: string;
};

export const EXCLUDED: Excluded[] = [
  {
    name: "var",
    why: "ambiguous hoisting, function-scoped",
    snippet: `// Use let / const (block-scoped).
let count = 0;
const PI = 3.14;`,
  },
  {
    name: "function",
    why: "use arrow functions, class methods, or object-literal methods",
    snippet: `// Arrow funcs, class & object-literal methods.
const add = (a, b) => a + b;
const ops = { sub(a, b) { return a - b; } };`,
  },
  {
    name: "==",
    why: "type coercion is a footgun",
    snippet: `// Use === (strict equality).
if (id === 0) { /* ... */ }
if (name === "alice") { /* ... */ }`,
  },
  {
    name: "!=",
    why: "type coercion is a footgun",
    snippet: `// Use !== (strict inequality).
if (id !== 0) { /* ... */ }
if (status !== "ok") { /* ... */ }`,
  },
  {
    name: "eval",
    why: "arbitrary code execution",
    snippet: `// Not available — sandbox-first runtime.
import handler from "./handler.js";
// Compose modules instead of stringly code.`,
  },
  {
    name: "with",
    why: "blocks static name resolution",
    snippet: `// Use destructuring.
const { x, y, z } = vector;
// All names resolved statically.`,
  },
  {
    name: "Function()",
    why: "dynamic-eval class of foot-guns",
    snippet: `// Not available — sandbox-first runtime.
import handler from "./handler.js";
// Use real modules, not stringly code.`,
  },
  {
    name: "arguments",
    why: "magic, non-array, function-scoped",
    snippet: `// Use rest parameters (real array).
const log = (...args) => {
  for (const a of args) console.log(a);
};`,
  },
];

export type FeatureIcon = "drop" | "shield" | "leaf" | "clock";

export const FEATURES: { icon: FeatureIcon; title: string; body: string }[] = [
  {
    icon: "drop",
    title: "A subset, not a superset",
    body: "Modern ECMAScript 2027+ — let/const, arrow functions, classes, modules, async/await — minus the redundant constructs.",
  },
  {
    icon: "shield",
    title: "Sandbox-first",
    body: "No eval, no global mutable state, GET/HEAD-only fetch, timeouts, and explicit global injection via JSON/TOML/YAML.",
  },
  {
    icon: "leaf",
    title: "Explicit over clever",
    body: "Named imports only. Strict equality only. Descriptive names over shortcuts — even at the cost of verbosity.",
  },
  {
    icon: "clock",
    title: "A modern standard library, batteries included",
    body: "Temporal for dates, and direct module imports of JSON, TOML, YAML, CSV, JSONL and Markdown.",
  },
];
