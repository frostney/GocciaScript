export type Builtin = { name: string; snippet: string };

export const BUILTINS: Builtin[] = [
  {
    name: "JSON5",
    snippet: `import { name, retries } from "./goccia.json5";

import { parse as parseJSON5 } from "goccia:json5";
const text = '{ name: "demo", retries: 3, }';
const cfg = parseJSON5(text);
// trailing commas, comments, unquoted keys`,
  },
  {
    name: "TOML",
    snippet: `import { server, features } from "./pkg.toml";

import { parse as parseTOML } from "goccia:toml";
const text = 'server = "api"\\nfeatures = ["cache"]';
const cfg = parseTOML(text);`,
  },
  {
    name: "YAML",
    snippet: `import { services } from "./manifest.yaml";

import { parse as parseYAML } from "goccia:yaml";
const text = 'services:\\n  - api\\n  - worker';
const data = parseYAML(text);
// flow + block, anchors, multi-doc`,
  },
  {
    name: "JSONL",
    snippet: `import { "0" as firstEvent } from "./events.jsonl";

import { parse as parseJSONL } from "goccia:jsonl";
const text = '{"event":"start"}\\n{"event":"stop"}';
const events = parseJSONL(text);`,
  },
  {
    name: "CSV",
    snippet: `import { "0" as firstRow } from "./data.csv";

import { parse as parseCSV } from "goccia:csv";
const text = 'name,price\\ncoffee,3.5';
const rows = parseCSV(text, {
  headers: true,
});  // → [{ name, price }, …]`,
  },
  {
    name: "TSV",
    snippet: `import { "0" as firstRow } from "./data.tsv";

import { parse as parseTSV } from "goccia:tsv";
const text = 'name\\tprice\\ncoffee\\t3.5';
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

export type ProfileDisabledFeature = {
  name: string;
  implemented: boolean;
  defaultProfile: "Disabled" | "Strict" | "Not installed";
  enablement: string;
  standardsSource: string;
  /** Why the recommended profile keeps it off (one short phrase). */
  why: string;
};

export const PROFILE_DISABLED_FEATURES: ProfileDisabledFeature[] = [
  {
    name: "var",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-var",
    standardsSource: "ECMA-262 core",
    why: "ambiguous hoisting, function-scoped",
  },
  {
    name: "function",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-function",
    standardsSource: "ECMA-262 core",
    why: "use arrow functions, class methods, or object-literal methods",
  },
  {
    name: "== / !=",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-loose-equality",
    standardsSource: "ECMA-262 core",
    why: "type coercion is a footgun",
  },
  {
    name: "ASI",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-asi",
    standardsSource: "ECMA-262 core",
    why: "explicit statement boundaries are the recommended default",
  },
  {
    name: "labels",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-label",
    standardsSource: "ECMA-262 core",
    why: "prefer explicit helpers or state",
  },
  {
    name: "for (;;)",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-traditional-for-loop",
    standardsSource: "ECMA-262 core",
    why: "prefer iterators, for...of, or array methods",
  },
  {
    name: "for...in",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-for-in-loop",
    standardsSource: "ECMA-262 core",
    why: "prefer explicit own-key enumeration",
  },
  {
    name: "while / do...while",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-while-loops",
    standardsSource: "ECMA-262 core",
    why: "prefer bounded iteration forms",
  },
  {
    name: "arguments",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-arguments-object",
    standardsSource: "ECMA-262 core",
    why: "magic, non-array, function-scoped",
  },
  {
    name: "non-strict Script",
    implemented: true,
    defaultProfile: "Strict",
    enablement: "--compat-non-strict-mode",
    standardsSource: "ECMA-262 core",
    why: "strict semantics are the recommended default",
  },
  {
    name: "with",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--compat-non-strict-mode",
    standardsSource: "ECMA-262 core",
    why: "blocks static name resolution",
  },
  {
    name: "eval",
    implemented: true,
    defaultProfile: "Not installed",
    enablement: "private --test262-host",
    standardsSource: "ECMA-262 core",
    why: "normal hosts do not expose dynamic source evaluation",
  },
  {
    name: "Function()",
    implemented: true,
    defaultProfile: "Disabled",
    enablement: "--unsafe-function-constructor",
    standardsSource: "ECMA-262 core",
    why: "dynamic source construction is an explicit unsafe opt-in",
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
    body: "AI-agent execution with seeded VFS snapshots, explicit modules and capabilities, structured results, and host-owned limits.",
  },
  {
    icon: "leaf",
    title: "Explicit over clever",
    body: "Strict equality and explicit imports by default; compatibility flags are reserved for conformance and legacy semantics.",
  },
  {
    icon: "clock",
    title: "A modern standard library, batteries included",
    body: "Temporal for dates, and direct module imports of JSON, JSON5, TOML, YAML, CSV, TSV, JSONL and Markdown.",
  },
];
