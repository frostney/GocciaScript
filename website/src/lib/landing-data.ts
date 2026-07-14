export type Builtin = { name: string; snippet: string };

export const BUILTINS: Builtin[] = [
  {
    name: "JSON5",
    snippet: `import { name, retries } from "./goccia.json5";
import { parse as parseJSON5 } from "goccia:json5";
const cfg = parseJSON5(text);
// trailing commas, comments, unquoted keys`,
  },
  {
    name: "TOML",
    snippet: `import { server, features } from "./pkg.toml";
import { parse as parseTOML } from "goccia:toml";
const cfg = parseTOML(text);`,
  },
  {
    name: "YAML",
    snippet: `import { services } from "./manifest.yaml";
import { parse as parseYAML } from "goccia:yaml";
const data = parseYAML(text);
// flow + block, anchors, multi-doc`,
  },
  {
    name: "JSONL",
    snippet: `import { "0" as firstEvent } from "./events.jsonl";
import { parse as parseJSONL } from "goccia:jsonl";
const events = parseJSONL(text);`,
  },
  {
    name: "CSV",
    snippet: `import { "0" as firstRow } from "./data.csv";
import { parse as parseCSV } from "goccia:csv";
const rows = parseCSV(text, {
  headers: true,
});  // → [{ name, price }, …]`,
  },
  {
    name: "TSV",
    snippet: `import { "0" as firstRow } from "./data.tsv";
import { parse as parseTSV } from "goccia:tsv";
const rows = parseTSV(text, {
  headers: true,
});  // tab-separated, same shape`,
  },
  {
    name: ".md",
    snippet: `import { content, metadata } from "./README.md";
// content is raw markdown text
console.log(content.length, metadata.fileName);`,
  },
  {
    name: "fetch",
    snippet: `const res = await fetch(url, {
  headers: { Accept: "application/json" },
});  // GET / HEAD only, opt-in hosts`,
  },
  {
    name: "fs (sandbox)",
    snippet: `import fs from "fs";
fs.mkdirSync("/out", { recursive: true });
fs.writeFileSync("/out/summary.txt", "ready\\n");`,
  },
  {
    name: "goccia",
    snippet: `import { $, runScript } from "goccia";
await $\`cat /out/summary.txt\`.text();
runScript("/child.js", { sandbox: true });`,
  },
  {
    name: "console",
    snippet: `console.log("total:", 6.5);
console.warn("deprecated");
console.error("oops");`,
  },
  {
    name: "SemVer",
    snippet: `import * as semver from "goccia:semver";
semver.inc("1.2.3", "minor");
semver.satisfies("1.3.0", "^1.0.0");`,
  },
  {
    name: "Import Maps",
    snippet: `// goccia.json
{ "imports": { "@app/": "./src/" } }

import { apiUrl } from "@app/config.json";`,
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
    snippet: `// Disabled by default; unsafe opt-in only.
// --unsafe-function-constructor
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
    title: "Modern by default",
    body: "Modern ECMAScript forms first — let/const, arrow functions, classes, modules, async/await.",
  },
  {
    icon: "shield",
    title: "Sandbox-first",
    body: "Seeded VFS snapshots, import-only fs/goccia modules, nested execution, explicit diffs, and host-owned limits.",
  },
  {
    icon: "leaf",
    title: "Explicit over clever",
    body: "Strict equality and explicit imports by default; compatibility flags are reserved for conformance and legacy semantics.",
  },
  {
    icon: "clock",
    title: "A modern standard library, batteries included",
    body: "Temporal for dates, and direct module imports of JSON, TOML, YAML, CSV, JSONL and Markdown.",
  },
];
