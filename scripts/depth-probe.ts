#!/usr/bin/env bun
/**
 * depth-probe.ts
 *
 * Runs depth and nesting probes against a GocciaRunner binary, in each
 * execution mode it supports, and classifies how each one ends: completed, a
 * catchable error, an engine fatal error, a crash, a timeout, or a kill by the
 * memory cap. A seeded fuzz mode composes the same shapes pseudo-randomly.
 *
 * MANUAL USE ONLY. This is a developer and agent tool; it is deliberately not
 * wired into CI, Lefthook, or any other automatic path. See
 * docs/contributing/tooling.md#depth-and-fuzz-probe.
 *
 * Every probe process runs under a hard memory cap and a timeout. Exponential
 * shapes (nested Proxies with native traps, issue #1441) and uncollected
 * garbage (issue #1442) have grown a runner past 8 GB and 27 GB on an agent
 * host; the cap is what keeps a probe from taking the host with it. On Linux
 * the cap is a systemd user scope (MemoryMax, no swap), else an address-space
 * rlimit through `ulimit -v`. Where neither works the tool refuses to run
 * unless --no-memory-cap is given.
 *
 * Usage:
 *   bun scripts/depth-probe.ts <runner> [options]
 *   npx tsx scripts/depth-probe.ts <runner> [options]
 *   bun scripts/depth-probe.ts --list
 *
 * Exit status: 0 when nothing crashed, hit a fatal error, or ended in an
 * unrecognised way; 1 when something did; 2 on a usage or setup error.
 * Timeouts and memory-cap kills are reported but do not fail the run.
 */

import { spawn, spawnSync } from "node:child_process";
import { accessSync, constants, mkdirSync, mkdtempSync, rmSync, statSync, writeFileSync } from "node:fs";
import { constants as osConstants, tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
// Relative to the working directory when inside it, else absolute.
const displayPath = (path: string): string => {
  const rel = relative(process.cwd(), path);
  return rel === "" ? "." : rel.startsWith("..") ? path : rel;
};

const SCRIPT = displayPath(fileURLToPath(import.meta.url));
// Replay commands use whichever runtime this run is on.
const RUNTIME = "bun" in process.versions ? ["bun"] : ["npx", "tsx"];

// ── Probe programs ─────────────────────────────────────────────────────

// Shared helpers every probe program starts with. Written in GocciaScript's
// default profile: arrows, const/let, for...of, strict equality.
const PRELUDE = `const nest = (depth, target, handler) => {
  let proxy = target;
  for (const _ of Array.from({ length: depth })) {
    proxy = new Proxy(proxy, handler === undefined ? {} : handler);
  }
  return proxy;
};
const handlerChain = (depth, target) => {
  let handler = {};
  for (const _ of Array.from({ length: depth })) {
    handler = new Proxy({}, handler);
  }
  return new Proxy(target, handler);
};
const protoChain = (depth, base) => {
  let object = base;
  for (const _ of Array.from({ length: depth })) {
    object = Object.create(object);
  }
  return object;
};
const deepArray = (depth) => {
  let array = [];
  for (const _ of Array.from({ length: depth })) {
    array = [array];
  }
  return array;
};
const deepObject = (depth) => {
  let object = {};
  for (const _ of Array.from({ length: depth })) {
    object = { o: object };
  }
  return object;
};
const bindChain = (depth, fn) => {
  let bound = fn;
  for (const _ of Array.from({ length: depth })) {
    bound = bound.bind(null);
  }
  return bound;
};
const describeError = (error) => {
  if (error !== null && typeof error === "object") {
    const ctor = error.constructor;
    const name = ctor !== undefined && ctor !== null && typeof ctor.name === "string" ? ctor.name : "Object";
    return name + ": " + String(error.message);
  }
  return typeof error + ": " + String(error);
};
const report = (run) => {
  try {
    console.log("PROBE-OK " + String(run()));
  } catch (error) {
    console.log("PROBE-THROW " + describeError(error));
  }
};
const check = (run, expected) => {
  try {
    const actual = String(run());
    console.log(actual === expected ? "PROBE-OK " + actual : "PROBE-WRONG expected " + expected + ", got " + actual);
  } catch (error) {
    console.log("PROBE-WRONG expected " + expected + ", threw " + describeError(error));
  }
};
const sanity = () => [JSON.stringify({ a: [1, { b: 2 }] }), nest(50, { x: 3 }).x, [3, 1, 2].sort().join("")].join(" ");
const SANITY = '{"a":[1,{"b":2}]} 3 123';
`;

type Growth = "linear" | "quadratic" | "exponential";

interface Probe {
  id: string;
  growth: Growth;
  summary: string;
  /** Fixed depths that replace the growth class defaults (e.g. a repeat count). */
  depths?: number[];
  body: (depth: number) => string;
}

// One probe per Proxy internal method: a nest of handler-less Proxies forwards
// the method down to the target, one native call per level (issue #1416).
const proxyForwarding: Array<[string, (d: number) => string]> = [
  ["get", (d) => `nest(${d}, { x: 1 }).x`],
  ["set", (d) => `Reflect.set(nest(${d}, {}), "y", 2)`],
  ["has", (d) => `"x" in nest(${d}, { x: 1 })`],
  ["deleteProperty", (d) => `delete nest(${d}, { x: 1 }).x`],
  ["defineProperty", (d) => `Reflect.defineProperty(nest(${d}, {}), "z", { value: 3 })`],
  ["getOwnPropertyDescriptor", (d) => `JSON.stringify(Object.getOwnPropertyDescriptor(nest(${d}, { x: 1 }), "x"))`],
  ["ownKeys", (d) => `Reflect.ownKeys(nest(${d}, { x: 1 })).length`],
  ["keys", (d) => `Object.keys(nest(${d}, { x: 1 })).join(",")`],
  ["isExtensible", (d) => `Object.isExtensible(nest(${d}, {}))`],
  ["isFrozen", (d) => `Object.isFrozen(nest(${d}, {}))`],
  ["preventExtensions", (d) => `Reflect.preventExtensions(nest(${d}, {}))`],
  ["getPrototypeOf", (d) => `Object.getPrototypeOf(nest(${d}, {})) === Object.prototype`],
  ["setPrototypeOf", (d) => `Reflect.setPrototypeOf(nest(${d}, {}), {})`],
  ["apply", (d) => `nest(${d}, () => 1)()`],
  ["construct", (d) => `typeof new (nest(${d}, class {}))()`],
];

const CATALOG: Probe[] = [
  ...proxyForwarding.map(([method, expression]): Probe => ({
    id: `proxy-${method}`,
    growth: "linear",
    summary: `Proxy nest without traps forwards ${method} to its target`,
    body: (d) => `report(() => ${expression(d)});\n`,
  })),
  {
    id: "proxy-handler-chain",
    growth: "linear",
    summary: "Proxy whose handler is a Proxy whose handler is a Proxy ...; read a property",
    body: (d) => `report(() => handlerChain(${d}, { x: 1 }).x);\n`,
  },
  {
    id: "proxy-js-get-trap",
    growth: "quadratic",
    summary: "Proxy nest with a JS get trap; each level's invariant check walks the rest of the nest",
    body: (d) => `report(() => nest(${d}, { x: 1 }, { get: (t, k) => Reflect.get(t, k) }).x);\n`,
  },
  {
    id: "proxy-native-ownKeys",
    growth: "exponential",
    summary: "Proxy nest with { ownKeys: Reflect.ownKeys }; invariant checks re-read the target (#1441)",
    body: (d) => `report(() => Reflect.ownKeys(nest(${d}, { x: 1 }, { ownKeys: Reflect.ownKeys })).length);\n`,
  },
  {
    id: "proxy-native-gopd",
    growth: "exponential",
    summary: "Proxy nest with { getOwnPropertyDescriptor: Reflect.getOwnPropertyDescriptor }",
    body: (d) =>
      `report(() => JSON.stringify(Object.getOwnPropertyDescriptor(nest(${d}, { x: 1 }, ` +
      `{ getOwnPropertyDescriptor: Reflect.getOwnPropertyDescriptor }), "x")));\n`,
  },
  {
    id: "proxy-js-ownKeys-trap",
    growth: "exponential",
    summary: "Proxy nest with a JS ownKeys trap on every level (#1441 trap-count shape)",
    body: (d) =>
      `report(() => Reflect.ownKeys(nest(${d}, { x: 1 }, { ownKeys: (t) => Reflect.ownKeys(t) })).length);\n`,
  },
  {
    id: "proxy-reflect-handler",
    growth: "exponential",
    summary: "Proxy nest using Reflect itself as the handler, so every trap is native; Object.keys",
    body: (d) => `report(() => Object.keys(nest(${d}, { x: 1 }, Reflect)).join(","));\n`,
  },
  {
    id: "recover-recursion",
    growth: "linear",
    // DEPTH is a repeat count here, not a nesting depth.
    depths: [1, 5],
    summary: "Overflow the JS stack DEPTH times (a count), catching each RangeError, then check the engine still works",
    body: (d) =>
      `const overflow = () => overflow();\n` +
      `report(() => Array.from({ length: ${d} }).map(() => { try { overflow(); return "none"; } catch (e) { return e.constructor.name; } }).join(","));\n` +
      `check(sanity, SANITY);\n`,
  },
  {
    id: "recover-proxy",
    growth: "linear",
    // 3,000 is past a Proxy forwarding bound of the size #1416 proposes, and
    // 30,000 is past the native stack without one.
    depths: [3000, 30000],
    summary: "Catch the error from a deep Proxy getOwnPropertyDescriptor twice, then check the engine still works",
    body: (d) =>
      `const deep = nest(${d}, { x: 1 });\n` +
      `report(() => JSON.stringify(Object.getOwnPropertyDescriptor(deep, "x")));\n` +
      `report(() => JSON.stringify(Object.getOwnPropertyDescriptor(deep, "x")));\n` +
      `check(sanity, SANITY);\n`,
  },
  {
    id: "proto-chain-get",
    growth: "linear",
    summary: "Read a missing property through a deep prototype chain (#1373)",
    body: (d) => `report(() => protoChain(${d}, {}).missing);\n`,
  },
  {
    id: "proto-chain-has",
    growth: "linear",
    summary: "`in` through a deep prototype chain",
    body: (d) => `report(() => "missing" in protoChain(${d}, {}));\n`,
  },
  {
    id: "proto-chain-set",
    growth: "linear",
    summary: "Assign through a deep prototype chain",
    body: (d) => `report(() => Reflect.set(protoChain(${d}, {}), "y", 1));\n`,
  },
  {
    id: "proto-chain-instanceof",
    growth: "linear",
    summary: "instanceof against a constructor missing from a deep prototype chain",
    body: (d) => `report(() => protoChain(${d}, {}) instanceof (class {}));\n`,
  },
  {
    id: "proto-chain-proxies",
    growth: "linear",
    summary: "Prototype chain alternating Object.create and Proxy links; read a missing property",
    body: (d) =>
      `report(() => { let o = {}; let i = 0; for (const _ of Array.from({ length: ${d} })) { o = i % 2 === 0 ? Object.create(o) : new Proxy(o, {}); i += 1; } return o.missing; });\n`,
  },
  {
    id: "recursion-js",
    growth: "linear",
    summary: "Plain JS recursion DEPTH deep",
    body: (d) => `const down = (n) => n === 0 ? 0 : 1 + down(n - 1);\nreport(() => down(${d}));\n`,
  },
  {
    id: "bind-chain",
    growth: "linear",
    summary: "Call a function bound DEPTH times",
    body: (d) => `report(() => bindChain(${d}, () => 1)());\n`,
  },
  {
    id: "class-extends-chain",
    growth: "linear",
    summary: "Construct the most derived of DEPTH chained subclasses",
    body: (d) =>
      `report(() => { let C = class {}; for (const _ of Array.from({ length: ${d} })) { const B = C; C = class extends B {}; } return typeof new C(); });\n`,
  },
  {
    id: "json-stringify-array",
    growth: "linear",
    summary: "JSON.stringify of an array nested DEPTH deep",
    body: (d) => `report(() => JSON.stringify(deepArray(${d})).length);\n`,
  },
  {
    id: "json-stringify-object",
    growth: "linear",
    summary: "JSON.stringify of an object nested DEPTH deep",
    body: (d) => `report(() => JSON.stringify(deepObject(${d})).length);\n`,
  },
  {
    id: "json-parse-array",
    growth: "linear",
    summary: "JSON.parse of a DEPTH-deep array literal",
    body: (d) => `report(() => typeof JSON.parse("[".repeat(${d}) + "]".repeat(${d})));\n`,
  },
  {
    id: "json-parse-object",
    growth: "linear",
    summary: "JSON.parse of a DEPTH-deep object literal",
    body: (d) => `report(() => typeof JSON.parse('{"o":'.repeat(${d}) + "1" + "}".repeat(${d})));\n`,
  },
  {
    id: "structured-clone-array",
    growth: "linear",
    summary: "structuredClone of a DEPTH-deep array, when the host has structuredClone",
    body: (d) =>
      `report(() => typeof structuredClone === "function" ? typeof structuredClone(deepArray(${d})) : "absent");\n`,
  },
  {
    id: "structured-clone-object",
    growth: "linear",
    summary: "structuredClone of a DEPTH-deep object, when the host has structuredClone",
    body: (d) =>
      `report(() => typeof structuredClone === "function" ? typeof structuredClone(deepObject(${d})) : "absent");\n`,
  },
  {
    id: "array-join",
    growth: "linear",
    summary: "String() of a DEPTH-deep array (Array.prototype.join recursion)",
    body: (d) => `report(() => String(deepArray(${d})).length);\n`,
  },
  {
    id: "array-flat",
    growth: "linear",
    summary: "flat(Infinity) of a DEPTH-deep array",
    body: (d) => `report(() => deepArray(${d}).flat(Infinity).length);\n`,
  },
  {
    id: "regexp-groups",
    growth: "linear",
    summary: "RegExp with DEPTH nested groups",
    body: (d) => `report(() => new RegExp("(".repeat(${d}) + "a" + ")".repeat(${d})).test("a"));\n`,
  },
  // Parser probes: the nesting is in the program text, so a parse-time error
  // ends the run before any report() and is classified from the runner's
  // own diagnostic.
  {
    id: "parse-parens",
    growth: "linear",
    summary: "Parse DEPTH nested parentheses",
    body: (d) => `report(() => ${"(".repeat(d)}1${")".repeat(d)});\n`,
  },
  {
    id: "parse-arrays",
    growth: "linear",
    summary: "Parse a DEPTH-deep array literal",
    body: (d) => `report(() => ${"[".repeat(d)}${"]".repeat(d)}.length);\n`,
  },
  {
    id: "parse-objects",
    growth: "linear",
    summary: "Parse a DEPTH-deep object literal",
    body: (d) => `report(() => typeof ${"{ o: ".repeat(d)}1${" }".repeat(d)});\n`,
  },
  {
    id: "parse-arrows",
    growth: "linear",
    summary: "Parse DEPTH nested arrow functions",
    body: (d) => `report(() => typeof (${"() => ".repeat(d)}1));\n`,
  },
  {
    id: "parse-blocks",
    growth: "linear",
    summary: "Parse DEPTH nested block statements",
    body: (d) => `report(() => { ${"{ ".repeat(d)}${"} ".repeat(d)}return 1; });\n`,
  },
  {
    id: "parse-calls",
    growth: "linear",
    summary: "Parse DEPTH nested calls",
    body: (d) => `const id = (v) => v;\nreport(() => ${"id(".repeat(d)}1${")".repeat(d)});\n`,
  },
  {
    id: "parse-unary",
    growth: "linear",
    summary: "Parse DEPTH chained unary operators",
    body: (d) => `report(() => ${"!".repeat(d)}true);\n`,
  },
  {
    id: "parse-ternary",
    growth: "linear",
    summary: "Parse DEPTH nested conditional expressions",
    body: (d) => `report(() => ${"true ? ".repeat(d)}1${" : 0".repeat(d)});\n`,
  },
  {
    id: "parse-templates",
    growth: "linear",
    summary: "Parse DEPTH nested template literals",
    body: (d) => `report(() => ${"`${".repeat(d)}1${"}`".repeat(d)});\n`,
  },
  {
    id: "parse-binary-chain",
    growth: "linear",
    summary: "Parse and evaluate a DEPTH-term left-associative + chain",
    body: (d) => `report(() => 1${" + 1".repeat(d)});\n`,
  },
];

// ── Fuzz programs ──────────────────────────────────────────────────────

// mulberry32: small, fast, and identical under Bun and Node.
const prng = (seed: number): (() => number) => {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

// Each program draws from its own stream, derived from (seed, index), so
// --seed S --index I replays one program without generating the others.
const programRandom = (seed: number, index: number): (() => number) =>
  prng(Math.imul(seed ^ 0x9e3779b9, 0x85ebca6b) ^ Math.imul(index + 1, 0xc2b2ae35));

type Kind = "object" | "array" | "callable" | "constructor";

interface FuzzLimits {
  maxDepth: number;
  maxQuadraticDepth: number;
  maxExponentialDepth: number;
}

interface FuzzProgram {
  id: string;
  shape: string;
  depth: number;
  source: string;
}

const LINEAR_STEPS = [1, 10, 100, 1000, 3000, 10000, 30000, 100000];
const QUADRATIC_STEPS = [1, 10, 100, 300, 1000, 3000];

interface Wrapper {
  name: string;
  growth: Growth;
  kinds: Kind[];
  wrap: (inner: string, depth: number) => string;
}

const WRAPPERS: Wrapper[] = [
  { name: "nest", growth: "linear", kinds: ["object", "array", "callable", "constructor"], wrap: (s, d) => `nest(${d}, ${s})` },
  { name: "nest[js-get]", growth: "quadratic", kinds: ["object", "array"], wrap: (s, d) => `nest(${d}, ${s}, { get: (t, k) => Reflect.get(t, k) })` },
  { name: "nest[native-ownKeys]", growth: "exponential", kinds: ["object", "array"], wrap: (s, d) => `nest(${d}, ${s}, { ownKeys: Reflect.ownKeys })` },
  { name: "nest[native-gopd]", growth: "exponential", kinds: ["object", "array"], wrap: (s, d) => `nest(${d}, ${s}, { getOwnPropertyDescriptor: Reflect.getOwnPropertyDescriptor })` },
  { name: "nest[Reflect]", growth: "exponential", kinds: ["object", "array", "callable", "constructor"], wrap: (s, d) => `nest(${d}, ${s}, Reflect)` },
  { name: "handlerChain", growth: "linear", kinds: ["object", "array", "callable", "constructor"], wrap: (s, d) => `handlerChain(${d}, ${s})` },
  { name: "protoChain", growth: "linear", kinds: ["object", "array"], wrap: (s, d) => `protoChain(${d}, ${s})` },
  { name: "bindChain", growth: "linear", kinds: ["callable"], wrap: (s, d) => `bindChain(${d}, ${s})` },
];

const OPERATIONS: Record<Kind, Array<[string, string]>> = {
  object: [
    ["get", `s.x`],
    ["get-missing", `s.missing`],
    ["set", `Reflect.set(s, "y", 2)`],
    ["has", `"x" in s`],
    ["delete", `Reflect.deleteProperty(s, "x")`],
    ["defineProperty", `Reflect.defineProperty(s, "z", { value: 3, configurable: true })`],
    ["getOwnPropertyDescriptor", `JSON.stringify(Object.getOwnPropertyDescriptor(s, "x"))`],
    ["ownKeys", `Reflect.ownKeys(s).length`],
    ["keys", `Object.keys(s).join(",")`],
    ["entries", `Object.entries(s).length`],
    ["spread", `Object.keys({ ...s }).length`],
    ["isExtensible", `Object.isExtensible(s)`],
    ["preventExtensions", `Reflect.preventExtensions(s)`],
    ["isFrozen", `Object.isFrozen(s)`],
    ["getPrototypeOf", `Object.getPrototypeOf(s) === null`],
    ["setPrototypeOf", `Reflect.setPrototypeOf(s, {})`],
    ["JSON.stringify", `String(JSON.stringify(s)).length`],
    ["String", `String(s).length`],
    ["structuredClone", `typeof structuredClone === "function" ? typeof structuredClone(s) : "absent"`],
  ],
  array: [],
  callable: [
    ["call", `s()`],
    ["typeof", `typeof s`],
    ["bind-call", `s.bind(null)()`],
    ["get-name", `typeof s.name`],
  ],
  constructor: [
    ["construct", `typeof new s()`],
    ["typeof", `typeof s`],
    ["get-prototype", `typeof s.prototype`],
  ],
};
OPERATIONS.array = [...OPERATIONS.object, ["flat", `s.flat(Infinity).length`], ["join", `s.join(",").length`]];

const pick = <T>(random: () => number, items: readonly T[]): T => items[Math.floor(random() * items.length)];

const linearDepth = (random: () => number, limits: FuzzLimits): number =>
  pick(random, LINEAR_STEPS.filter((step) => step <= limits.maxDepth));

const generateProgram = (seed: number, index: number, limits: FuzzLimits): FuzzProgram => {
  const random = programRandom(seed, index);
  let maxDepth = 0;
  const base = pick(random, ["plain", "deepArray", "deepObject", "arrow", "class"] as const);
  let kind: Kind;
  let expression: string;
  let shape: string;
  switch (base) {
    case "plain":
      [kind, expression, shape] = ["object", `({ x: 1, [Symbol.iterator]: 2 })`, "{x}"];
      break;
    case "arrow":
      [kind, expression, shape] = ["callable", `(() => 1)`, "arrow"];
      break;
    case "class":
      [kind, expression, shape] = ["constructor", `(class {})`, "class"];
      break;
    default: {
      const depth = linearDepth(random, limits);
      maxDepth = depth;
      kind = base === "deepArray" ? "array" : "object";
      [expression, shape] = [`${base}(${depth})`, `${base}(${depth})`];
    }
  }

  // Exponential wrappers share one depth budget, so stacking two of them
  // cannot multiply past --max-exponential-depth.
  let exponentialBudget = limits.maxExponentialDepth;
  const layers = 1 + Math.floor(random() * 3);
  for (const _ of Array.from({ length: layers })) {
    const candidates = WRAPPERS.filter(
      (w) => w.kinds.includes(kind) && (w.growth !== "exponential" || exponentialBudget > 0),
    );
    const wrapper = pick(random, candidates);
    let depth: number;
    if (wrapper.growth === "exponential") {
      depth = 1 + Math.floor(random() * exponentialBudget);
      exponentialBudget -= depth;
    } else if (wrapper.growth === "quadratic") {
      depth = pick(random, QUADRATIC_STEPS.filter((step) => step <= Math.min(limits.maxDepth, limits.maxQuadraticDepth)));
    } else {
      depth = linearDepth(random, limits);
    }
    maxDepth = Math.max(maxDepth, depth);
    expression = wrapper.wrap(expression, depth);
    shape = `${wrapper.name}(${depth}) > ${shape}`;
    // A prototype chain over anything is an ordinary object.
    if (wrapper.name === "protoChain") kind = "object";
  }

  const [operationName, operation] = pick(random, OPERATIONS[kind]);
  const twice = random() < 0.3;
  const recover = random() < 0.4;
  let source = `let s;\nreport(() => { s = ${expression}; return "built"; });\n`;
  source += `report(() => ${operation});\n`;
  if (twice) source += `report(() => ${operation});\n`;
  if (recover) source += `check(sanity, SANITY);\n`;
  shape += ` | ${operationName}${twice ? " x2" : ""}${recover ? " +recover" : ""}`;
  return { id: `fuzz:${seed}:${index}`, shape, depth: maxDepth, source };
};

// ── Options ────────────────────────────────────────────────────────────

const USAGE = `usage: bun ${SCRIPT} <runner> [options]
       npx tsx ${SCRIPT} <runner> [options]
       bun ${SCRIPT} --list

Runs depth and nesting probes against a GocciaRunner binary under a memory cap
and a timeout. Manual use only; see docs/contributing/tooling.md.

Selection:
  --list                       List the catalog and exit
  --probe=a,b                  Only these probes (an exact id, or part of one)
  --modes=a,b                  Execution modes to probe (default interpreted,bytecode);
                               a mode the runner rejects is reported and skipped
  --mode=NAME                  One execution mode; same as --modes=NAME
  --depth=N                    Run every selected probe at exactly N
  --depths=a,b                 Depths for linear probes (default 1000,30000)
  --quadratic-depths=a,b       Depths for quadratic probes (default 300,3000)
  --exponential-depths=a,b     Depths for exponential probes (default 6,10)

Fuzz mode (instead of the catalog):
  --fuzz                       Generate programs from catalog shapes
  --seed=N                     Seed (default: random, always printed)
  --count=N                    Programs to generate (default 50)
  --index=N                    Replay only program N of the seed
  --max-depth=N                Largest linear depth to draw (default 30000)
  --max-quadratic-depth=N      Largest quadratic depth to draw (default 1000)
  --max-exponential-depth=N    Exponential depth budget per program (default 10)

Limits:
  --memory=SIZE                Memory cap per probe process (default 2G; K/M/G/T)
  --timeout=SECONDS            Timeout per probe process (default 60)
  --memory-cap=auto|systemd|rlimit   How to cap memory (default auto)
  --no-memory-cap              Run uncapped when no cap works on this host (a cap that works is still used)

Output:
  --keep                       Write findings, timeouts and memory-cap kills to --out
  --out=DIR                    Directory for kept programs (default tmp/depth-probe; implies --keep)
  --runner-arg=ARG             Extra argument for the runner (repeatable), e.g. --runner-arg=--max-stack=0
  --verbose                    Print the runner's output for every non-completed probe
  --help                       Show this help
`;

const DEFAULTS = {
  depths: { linear: [1000, 30000], quadratic: [300, 3000], exponential: [6, 10] } as Record<Growth, number[]>,
  count: 50,
  limits: { maxDepth: 30000, maxQuadraticDepth: 1000, maxExponentialDepth: 10 },
  memoryBytes: 2 * 1024 ** 3,
  timeoutSeconds: 60,
};

interface Options {
  runner: string;
  list: boolean;
  probes: string[];
  modes: string[];
  depth?: number;
  depths: Record<Growth, number[]>;
  fuzz: boolean;
  seed: number;
  count: number;
  index?: number;
  limits: FuzzLimits;
  memoryBytes: number;
  timeoutSeconds: number;
  capMethod: "auto" | "systemd" | "rlimit";
  allowUncapped: boolean;
  keep: boolean;
  out: string;
  runnerArgs: string[];
  verbose: boolean;
}

const fail = (message: string): never => {
  console.error(`depth-probe: ${message}`);
  process.exit(2);
};

const parseInteger = (name: string, raw: string, min: number): number => {
  const value = raw.trim() === "" ? NaN : Number(raw);
  if (!Number.isInteger(value) || value < min) fail(`${name} must be an integer >= ${min}, got: ${raw}`);
  return value;
};

const parseList = (name: string, raw: string): number[] => raw.split(",").map((part) => parseInteger(name, part.trim(), 0));

const parseSize = (raw: string): number => {
  const match = /^(\d+(?:\.\d+)?)\s*([KMGT]?)(?:i?B)?$/i.exec(raw.trim());
  if (!match) fail(`--memory must look like 512M or 2G, got: ${raw}`);
  const scale = { "": 1, K: 1024, M: 1024 ** 2, G: 1024 ** 3, T: 1024 ** 4 }[match![2].toUpperCase() as "" | "K" | "M" | "G" | "T"];
  // Whole pages, so the cgroup reports back exactly the limit that was set.
  const bytes = Math.floor((Number(match![1]) * scale) / 4096) * 4096;
  if (bytes < 16 * 1024 ** 2) fail(`--memory must be at least 16M, got: ${raw}`);
  return bytes;
};

const parseOptions = (argv: string[]): Options => {
  const options: Options = {
    runner: "",
    list: false,
    probes: [],
    modes: ["interpreted", "bytecode"],
    depths: structuredClone(DEFAULTS.depths),
    fuzz: false,
    seed: Math.floor(Math.random() * 2 ** 31),
    count: DEFAULTS.count,
    limits: { ...DEFAULTS.limits },
    memoryBytes: DEFAULTS.memoryBytes,
    timeoutSeconds: DEFAULTS.timeoutSeconds,
    capMethod: "auto",
    allowUncapped: false,
    keep: false,
    out: join(ROOT, "tmp", "depth-probe"),
    runnerArgs: [],
    verbose: false,
  };
  for (const arg of argv) {
    const eq = arg.indexOf("=");
    const [name, value] = eq === -1 ? [arg, ""] : [arg.slice(0, eq), arg.slice(eq + 1)];
    switch (name) {
      case "--help":
      case "-h":
        console.log(USAGE);
        process.exit(0);
      case "--list": options.list = true; break;
      case "--probe": options.probes = value.split(",").filter(Boolean); break;
      // Mode names are passed through to the runner, which decides which it
      // supports; see the preflight in main().
      case "--mode":
      case "--modes": {
        const modes = value.split(",").map((mode) => mode.trim());
        if (modes.some((mode) => !/^[a-z][a-z0-9-]*$/.test(mode))) fail(`${name} takes mode names such as bytecode, got: ${value}`);
        options.modes = [...new Set(modes)];
        if (name === "--mode" && options.modes.length !== 1) fail(`--mode takes one mode; use --modes=${value}`);
        break;
      }
      case "--depth": options.depth = parseInteger(name, value, 0); break;
      case "--depths": options.depths.linear = parseList(name, value); break;
      case "--quadratic-depths": options.depths.quadratic = parseList(name, value); break;
      case "--exponential-depths": options.depths.exponential = parseList(name, value); break;
      case "--fuzz": options.fuzz = true; break;
      case "--seed": options.seed = parseInteger(name, value, 0); break;
      case "--count": options.count = parseInteger(name, value, 1); break;
      case "--index": options.index = parseInteger(name, value, 0); break;
      case "--max-depth": options.limits.maxDepth = parseInteger(name, value, 1); break;
      case "--max-quadratic-depth": options.limits.maxQuadraticDepth = parseInteger(name, value, 1); break;
      case "--max-exponential-depth": options.limits.maxExponentialDepth = parseInteger(name, value, 0); break;
      case "--memory": options.memoryBytes = parseSize(value); break;
      case "--timeout": options.timeoutSeconds = parseInteger(name, value, 1); break;
      case "--memory-cap":
        if (!["auto", "systemd", "rlimit"].includes(value)) fail(`--memory-cap must be auto, systemd or rlimit, got: ${value}`);
        options.capMethod = value as Options["capMethod"];
        break;
      case "--no-memory-cap": options.allowUncapped = true; break;
      case "--keep": options.keep = true; break;
      case "--out": options.out = resolve(value); options.keep = true; break;
      case "--runner-arg": options.runnerArgs.push(value); break;
      case "--verbose": options.verbose = true; break;
      default:
        if (arg.startsWith("-")) fail(`unknown option ${arg}\n\n${USAGE}`);
        if (options.runner) fail(`only one runner path is accepted, got ${options.runner} and ${arg}`);
        options.runner = resolve(arg);
    }
  }
  if (options.allowUncapped && options.capMethod !== "auto") fail("--no-memory-cap cannot be combined with --memory-cap");
  if (options.index !== undefined && !options.fuzz) fail("--index needs --fuzz");
  return options;
};

// ── Memory cap and process launch ──────────────────────────────────────

interface Launcher {
  description: string;
  prefix: string[];
  method: "systemd" | "rlimit" | "none";
  usesTimeoutCommand: boolean;
}

const hasCommand = (command: string): boolean =>
  spawnSync("sh", ["-c", `command -v ${command}`], { stdio: "ignore" }).status === 0;

// Inside the sandbox shell: no core files where the kernel honours RLIMIT_CORE,
// and an empty coredump_filter where a piped core_pattern ignores it (systemd-
// coredump), so a crash records a few KB instead of the runner's whole heap.
const CORE_LIMITS = `ulimit -c 0 2>/dev/null; if [ -w /proc/self/coredump_filter ]; then echo 0 > /proc/self/coredump_filter; fi; `;

const systemdPrefix = (bytes: number, properties: string[]): string[] => [
  "systemd-run", "--user", "--scope", "-q", "-p", `MemoryMax=${bytes}`, "-p", "MemorySwapMax=0",
  ...properties.flatMap((property) => ["-p", property]), "--",
];

// Returns a systemd-run prefix whose scope really carries the limit, or
// undefined: on a host without a user manager, on cgroup v1, or without the
// memory controller delegated to the user, systemd-run can start the command
// with no cap at all. OOMPolicy=continue (systemd 253+) keeps systemd from
// stopping the scope with SIGTERM after the OOM kill, so the probe sees the
// runner's SIGKILL; older versions reject it and run without it.
const findSystemdPrefix = (bytes: number): string[] | undefined => {
  if (process.platform !== "linux" || !hasCommand("systemd-run")) return undefined;
  for (const properties of [["OOMPolicy=continue"], []]) {
    const prefix = systemdPrefix(bytes, properties);
    const check = spawnSync(
      prefix[0],
      [...prefix.slice(1), "sh", "-c", `cd "/sys/fs/cgroup$(sed -n 's/^0:://p' /proc/self/cgroup)" && cat memory.max memory.swap.max`],
      { encoding: "utf8", timeout: 30000 },
    );
    if (check.status === 0 && check.stdout.trim().split(/\s+/).join(" ") === `${bytes} 0`) return prefix;
  }
  return undefined;
};

// The shell must accept the limit, and the kernel must enforce it: some
// systems take `ulimit -v` without applying it. Under a 64 MiB limit dd must
// get a 1 MiB buffer and be refused a 256 MiB one.
const rlimitCapWorks = (kib: number): boolean => {
  const sh = (script: string): number | null => spawnSync("sh", ["-c", script], { stdio: "ignore" }).status;
  const dd = (bytes: number): string => `dd if=/dev/zero of=/dev/null bs=${bytes} count=1`;
  return (
    sh(`ulimit -v ${kib} && [ "$(ulimit -v)" = "${kib}" ]`) === 0 &&
    sh(`ulimit -v 65536 && ${dd(1024 ** 2)}`) === 0 &&
    sh(`ulimit -v 65536 && ${dd(256 * 1024 ** 2)}`) !== 0
  );
};

const formatBytes = (bytes: number): string => {
  const units = ["B", "KiB", "MiB", "GiB", "TiB"];
  let value = bytes;
  let unit = 0;
  while (value >= 1024 && unit < units.length - 1) {
    value /= 1024;
    unit += 1;
  }
  return `${Number.isInteger(value) ? value : value.toFixed(1)} ${units[unit]}`;
};

const createLauncher = (options: Options): Launcher => {
  const timeoutCommand = hasCommand("timeout");
  const timeoutPrefix = timeoutCommand ? ["timeout", "-k", "5", String(options.timeoutSeconds)] : [];
  const kib = Math.floor(options.memoryBytes / 1024);
  const sandbox = (limits: string): string[] => ["sh", "-c", `${limits}exec "$@"`, "depth-probe"];
  const wantSystemd = options.capMethod === "auto" || options.capMethod === "systemd";
  const systemd = wantSystemd ? findSystemdPrefix(options.memoryBytes) : undefined;
  if (systemd) {
    return {
      description: `systemd scope MemoryMax=${formatBytes(options.memoryBytes)}, no swap`,
      prefix: [...systemd, ...sandbox(CORE_LIMITS), ...timeoutPrefix],
      method: "systemd",
      usesTimeoutCommand: timeoutCommand,
    };
  }
  if (options.capMethod === "systemd") fail("--memory-cap=systemd: systemd-run --user could not apply MemoryMax on this host");
  if ((options.capMethod === "auto" || options.capMethod === "rlimit") && rlimitCapWorks(kib)) {
    return {
      description: `address-space rlimit ${formatBytes(kib * 1024)} (ulimit -v)`,
      prefix: [...sandbox(`ulimit -v ${kib}; ${CORE_LIMITS}`), ...timeoutPrefix],
      method: "rlimit",
      usesTimeoutCommand: timeoutCommand,
    };
  }
  if (options.capMethod === "rlimit") fail("--memory-cap=rlimit: ulimit -v is not supported on this host");
  if (!options.allowUncapped) {
    fail(
      "no memory cap is available on this host (neither systemd-run --user with MemoryMax nor ulimit -v works).\n" +
        "An uncapped probe can exhaust the host's memory. Pass --no-memory-cap to run anyway.",
    );
  }
  return {
    description: "NONE (no cap works on this host; --no-memory-cap)",
    prefix: [...sandbox(CORE_LIMITS), ...timeoutPrefix],
    method: "none",
    usesTimeoutCommand: timeoutCommand,
  };
};

// ── Running and classifying ────────────────────────────────────────────

type Outcome = "completed" | "error" | "fatal" | "crash" | "timeout" | "memory-cap" | "unexpected";

const FINDINGS: Outcome[] = ["fatal", "crash", "unexpected"];
const KEPT: Outcome[] = [...FINDINGS, "timeout", "memory-cap"];

interface RunResult {
  outcome: Outcome;
  detail: string;
  seconds: number;
  output: string;
}

const OUTPUT_LIMIT = 64 * 1024;
let activeChild: ReturnType<typeof spawn> | undefined;
let removeWorkDir = (): void => {};

const killGroup = (pid: number | undefined): void => {
  if (pid === undefined) return;
  try {
    process.kill(-pid, "SIGKILL");
  } catch {
    // Already gone.
  }
};

const runProgram = (launcher: Launcher, options: Options, file: string, mode: string): Promise<RunResult> =>
  new Promise((resolveRun) => {
    const start = process.hrtime.bigint();
    const [command, ...args] = [...launcher.prefix, options.runner, file, `--mode=${mode}`, ...options.runnerArgs];
    // A process group of its own, so a timeout or Ctrl-C kills the runner
    // and not just the wrapper around it.
    const child = spawn(command, args, { stdio: ["ignore", "pipe", "pipe"], detached: true });
    activeChild = child;
    let output = "";
    const collect = (chunk: Buffer): void => {
      if (output.length < OUTPUT_LIMIT) output += chunk.toString("utf8");
    };
    child.stdout!.on("data", collect);
    child.stderr!.on("data", collect);
    let backstopFired = false;
    // Backstop for hosts without coreutils timeout, and for a wrapper that
    // does not exit after its own deadline.
    const backstop = setTimeout(
      () => {
        backstopFired = true;
        killGroup(child.pid);
      },
      (options.timeoutSeconds + (launcher.usesTimeoutCommand ? 15 : 0)) * 1000,
    );
    child.on("error", (error) => {
      clearTimeout(backstop);
      activeChild = undefined;
      resolveRun({ outcome: "unexpected", detail: `could not start: ${error.message}`, seconds: 0, output });
    });
    child.on("close", (code, signal) => {
      clearTimeout(backstop);
      activeChild = undefined;
      const seconds = Number(process.hrtime.bigint() - start) / 1e9;
      resolveRun({ ...classify(launcher, options, code, signal, seconds, backstopFired, output), seconds, output });
    });
  });

const firstLine = (text: string, pattern: RegExp): RegExpMatchArray | null => {
  for (const line of text.split("\n")) {
    const match = pattern.exec(line.trim());
    if (match) return match;
  }
  return null;
};

const classify = (
  launcher: Launcher,
  options: Options,
  code: number | null,
  signal: NodeJS.Signals | null,
  seconds: number,
  backstopFired: boolean,
  output: string,
): { outcome: Outcome; detail: string } => {
  // timeout(1) re-raises the runner's fatal signal on itself; when it cannot,
  // it exits 128+N, as a shell would.
  const signalName =
    signal ??
    (code !== null && code > 128 && code < 160
      ? (Object.entries(osConstants.signals).find(([, number]) => number === code - 128)?.[0] as NodeJS.Signals | undefined) ?? null
      : null);
  const timedOut =
    backstopFired ||
    (launcher.usesTimeoutCommand && code === 124) ||
    (signalName === "SIGKILL" && seconds >= options.timeoutSeconds);
  if (timedOut) return { outcome: "timeout", detail: `no result within ${options.timeoutSeconds}s` };
  // Under the systemd cap, the cgroup's OOM killer is what sends SIGKILL to a
  // probe before its deadline. An rlimit never does; there it is an outside
  // kill, such as the host's OOM killer.
  if (signalName === "SIGKILL") {
    return launcher.method === "systemd"
      ? { outcome: "memory-cap", detail: `SIGKILL under the ${formatBytes(options.memoryBytes)} cap` }
      : { outcome: "unexpected", detail: "SIGKILL from outside the probe (the host's OOM killer?)" };
  }
  if (signalName !== null) return { outcome: "crash", detail: signalName };
  // Under an address-space rlimit the cap shows up as a refused allocation,
  // and the runner does not always survive that cleanly: it may report
  // "Out of memory", an access violation, or die in FPC's exception handling
  // (exit 217) with no output. Those endings are ambiguous under an rlimit,
  // so they are reported as the cap; confirm one under the systemd cap or a
  // larger --memory before treating it as a crash.
  if (launcher.method === "rlimit") {
    const refused =
      /out of memory|Runtime error 203/i.test(output) ||
      code === 203 ||
      /^Fatal error: Access violation/m.test(output) ||
      (code === 217 && output.trim() === "");
    if (refused) {
      return {
        outcome: "memory-cap",
        detail:
          `allocation refused under the ${formatBytes(options.memoryBytes)} rlimit ` +
          `(${firstLine(output, /^(?:PROBE-THROW )?(.*(?:Out of memory|Fatal error|Runtime error).*)$/i)?.[1] ?? `exit ${code}`})`,
      };
    }
  }
  const fatal = firstLine(output, /^Fatal error: (.*)$/);
  if (fatal) return { outcome: "fatal", detail: `Fatal error: ${fatal[1]}` };
  // FPC prints this for an exception nothing handled (216 access violation,
  // 202 stack overflow, ...).
  const runtimeError = firstLine(output, /^Runtime error (\d+)/);
  if (runtimeError) return { outcome: "crash", detail: `FPC runtime error ${runtimeError[1]}` };
  if (code === 0) {
    const steps = output.split("\n").filter((line) => line.startsWith("PROBE-"));
    if (steps.length === 0) return { outcome: "unexpected", detail: "exit 0 without probe output" };
    const wrong = steps.find((line) => line.startsWith("PROBE-WRONG"));
    if (wrong) return { outcome: "unexpected", detail: `after the error: ${wrong.slice("PROBE-WRONG ".length)}` };
    const thrown = steps.filter((line) => line.startsWith("PROBE-THROW")).map((line) => line.slice("PROBE-THROW ".length));
    const results = steps.filter((line) => line.startsWith("PROBE-OK")).map((line) => line.slice("PROBE-OK ".length));
    if (thrown.length > 0) {
      const after = results.filter((result) => result !== "built");
      return { outcome: "error", detail: thrown[0] + (after.length > 0 ? ` (then: ${after.join(", ")})` : "") };
    }
    return { outcome: "completed", detail: results.filter((result) => result !== "built").join(", ") };
  }
  if (code === 1) {
    // An uncaught error, typically a parse-time one, as printed by the runner.
    const uncaught = firstLine(output, /^([A-Z][A-Za-z0-9_$]*(?:Error|Exception)): (.*)$/);
    if (uncaught) return { outcome: "error", detail: `${uncaught[1]}: ${uncaught[2]} (uncaught)` };
  }
  const tail = output.trim().split("\n").slice(-1)[0] ?? "";
  return { outcome: "unexpected", detail: `exit ${code}${tail ? `: ${tail}` : ""}` };
};

// ── Main ───────────────────────────────────────────────────────────────

interface Job {
  id: string;
  growth: string;
  depth: number;
  source: string;
  shape?: string;
  replay: (mode: string) => string;
}

const truncate = (text: string, width: number): string =>
  text.length <= width ? text : text.slice(0, width - 1) + "…";

const GROWTH_LABEL: Record<string, string> = { linear: "lin", quadratic: "quad", exponential: "EXP", fuzz: "fuzz" };

const quote = (arg: string): string => (/^[\w@%+=:,./-]+$/.test(arg) ? arg : `'${arg.replaceAll("'", "'\\''")}'`);

const main = async (): Promise<void> => {
  const options = parseOptions(process.argv.slice(2));

  if (options.list) {
    console.log(`${"probe".padEnd(32)}${"growth".padEnd(13)}${"default depths".padEnd(16)}summary`);
    for (const probe of CATALOG) {
      const depths = probe.depths ?? options.depths[probe.growth];
      console.log(`${probe.id.padEnd(32)}${probe.growth.padEnd(13)}${depths.join(",").padEnd(16)}${probe.summary}`);
    }
    return;
  }

  if (!options.runner) fail(`a runner path is required\n\n${USAGE}`);
  try {
    accessSync(options.runner, constants.X_OK);
    if (!statSync(options.runner).isFile()) throw new Error("not a file");
  } catch {
    fail(`runner is not an executable file: ${options.runner}`);
  }

  const launcher = createLauncher(options);
  // Everything besides the probe selection that shapes a run, so a replay
  // runs under the same limits and runner options.
  const commonFlags = [
    options.memoryBytes !== DEFAULTS.memoryBytes ? `--memory=${options.memoryBytes}` : "",
    options.timeoutSeconds !== DEFAULTS.timeoutSeconds ? `--timeout=${options.timeoutSeconds}` : "",
    options.capMethod !== "auto" ? `--memory-cap=${options.capMethod}` : "",
    options.allowUncapped ? "--no-memory-cap" : "",
    ...options.runnerArgs.map((arg) => `--runner-arg=${arg}`),
  ];
  const replayCommand = (args: string[], mode: string): string =>
    [...RUNTIME, SCRIPT, displayPath(options.runner), ...args, `--mode=${mode}`,
      ...commonFlags.filter(Boolean)]
      .map(quote)
      .join(" ");

  const jobs: Job[] = [];
  if (options.fuzz) {
    const indices = options.index !== undefined
      ? [options.index]
      : Array.from({ length: options.count }, (_, i) => i);
    const limitFlags = [
      options.limits.maxDepth !== DEFAULTS.limits.maxDepth ? `--max-depth=${options.limits.maxDepth}` : "",
      options.limits.maxQuadraticDepth !== DEFAULTS.limits.maxQuadraticDepth
        ? `--max-quadratic-depth=${options.limits.maxQuadraticDepth}`
        : "",
      options.limits.maxExponentialDepth !== DEFAULTS.limits.maxExponentialDepth
        ? `--max-exponential-depth=${options.limits.maxExponentialDepth}`
        : "",
    ].filter(Boolean);
    for (const index of indices) {
      const program = generateProgram(options.seed, index, options.limits);
      jobs.push({
        id: program.id,
        growth: "fuzz",
        depth: program.depth,
        source: program.source,
        shape: program.shape,
        replay: (mode) => replayCommand(["--fuzz", `--seed=${options.seed}`, `--index=${index}`, ...limitFlags], mode),
      });
    }
  } else {
    // A filter that names a probe exactly selects only that probe, so the
    // replay command for proxy-get does not also run proxy-getPrototypeOf.
    const matches = (id: string, filter: string): boolean =>
      CATALOG.some((probe) => probe.id === filter) ? id === filter : id.includes(filter);
    const selected = CATALOG.filter(
      (probe) => options.probes.length === 0 || options.probes.some((filter) => matches(probe.id, filter)),
    );
    if (selected.length === 0) fail(`no probe matches --probe=${options.probes.join(",")}; see --list`);
    for (const probe of selected) {
      const depths = options.depth !== undefined ? [options.depth] : probe.depths ?? options.depths[probe.growth];
      for (const depth of depths) {
        jobs.push({
          id: probe.id,
          growth: probe.growth,
          depth,
          source: probe.body(depth),
          replay: (mode) => replayCommand([`--probe=${probe.id}`, `--depth=${depth}`], mode),
        });
      }
    }
  }

  console.log(`runner:      ${options.runner}`);
  console.log(`memory cap:  ${launcher.description}`);
  console.log(`timeout:     ${options.timeoutSeconds}s per process${launcher.usesTimeoutCommand ? "" : " (enforced by this script; timeout(1) not found)"}`);
  if (options.fuzz) console.log(`fuzz seed:   ${options.seed} (${jobs.length} program${jobs.length === 1 ? "" : "s"}; replay with --fuzz --seed=${options.seed})`);
  if (options.runnerArgs.length > 0) console.log(`runner args: ${options.runnerArgs.join(" ")}`);

  const work = mkdtempSync(join(tmpdir(), "goccia-depth-probe-"));
  removeWorkDir = () => rmSync(work, { recursive: true, force: true });
  for (const [signal, status] of [["SIGINT", 130], ["SIGTERM", 143], ["SIGHUP", 129]] as const) {
    process.on(signal, () => {
      killGroup(activeChild?.pid);
      removeWorkDir();
      process.exit(status);
    });
  }

  // Preflight: run a trivial program in each requested mode. A mode the
  // runner rejects as an option (for example after an executor is removed)
  // is skipped and reported, not counted as a finding; a runner that cannot
  // run the trivial program at all is a setup error.
  const preflight = join(work, "preflight.js");
  writeFileSync(preflight, PRELUDE + "report(() => 1 + 1);\n");
  const modes: string[] = [];
  const skipped: string[] = [];
  for (const mode of options.modes) {
    const result = await runProgram(launcher, options, preflight, mode);
    if (result.outcome === "completed" && result.detail === "2") {
      modes.push(mode);
      continue;
    }
    const message = result.output.trim().split("\n")[0] ?? "";
    if (/\bmode\b/i.test(message) && result.outcome !== "crash" && result.outcome !== "timeout") {
      skipped.push(`${mode} (${truncate(message, 100)})`);
      continue;
    }
    removeWorkDir();
    fail(`the runner cannot run a trivial program in --mode=${mode}: ${result.outcome}: ${result.detail}`);
  }
  console.log(`modes:       ${modes.join(", ") || "none"}`);
  if (skipped.length > 0) console.log(`skipped:     ${skipped.join("; ")}: not supported by this runner`);
  console.log("");
  if (modes.length === 0) {
    removeWorkDir();
    fail("the runner supports none of the requested modes");
  }

  const header = `${"probe".padEnd(32)}${"growth".padEnd(7)}${"depth".padEnd(8)}${"mode".padEnd(12)}${"outcome".padEnd(12)}${"time".padEnd(9)}detail`;
  console.log(header);
  console.log("-".repeat(header.length + 20));

  const counts = new Map<Outcome, number>();
  const findings: string[] = [];
  const kept: string[] = [];
  try {
    for (const [jobIndex, job] of jobs.entries()) {
      const source = PRELUDE + job.source;
      const file = join(work, `probe-${jobIndex}.js`);
      writeFileSync(file, source);
      if (job.shape) console.log(`${job.id}  ${job.shape}`);
      for (const mode of modes) {
        const result = await runProgram(launcher, options, file, mode);
        counts.set(result.outcome, (counts.get(result.outcome) ?? 0) + 1);
        console.log(
          truncate(job.id, 31).padEnd(32) +
            (GROWTH_LABEL[job.growth] ?? job.growth).padEnd(7) +
            String(job.depth).padEnd(8) +
            mode.padEnd(12) +
            result.outcome.padEnd(12) +
            `${result.seconds.toFixed(2)}s`.padEnd(9) +
            truncate(result.detail.replaceAll("\n", " "), 90),
        );
        if (options.verbose && result.outcome !== "completed") {
          for (const line of result.output.trim().split("\n").slice(0, 20)) console.log(`    | ${truncate(line, 200)}`);
        }
        if (FINDINGS.includes(result.outcome)) {
          findings.push(`${result.outcome.padEnd(11)} ${job.id} depth ${job.depth} ${mode}: ${result.detail}\n    replay: ${job.replay(mode)}`);
        }
        if (options.keep && KEPT.includes(result.outcome)) {
          mkdirSync(options.out, { recursive: true });
          const name = `${job.id.replaceAll(":", "-")}-d${job.depth}-${mode}.js`;
          const target = join(options.out, name);
          writeFileSync(
            target,
            `// depth-probe: ${result.outcome}: ${result.detail.replaceAll("\n", " ")}\n` +
              (job.shape ? `// shape: ${job.shape}\n` : "") +
              `// runner: ${options.runner} --mode=${mode}${options.runnerArgs.length ? " " + options.runnerArgs.join(" ") : ""}\n` +
              `// memory cap: ${launcher.description}; timeout ${options.timeoutSeconds}s\n` +
              `// replay: ${job.replay(mode)}\n` +
              source,
          );
          kept.push(target);
        }
      }
    }
  } finally {
    removeWorkDir();
  }

  console.log("");
  const order: Outcome[] = ["completed", "error", "timeout", "memory-cap", "fatal", "crash", "unexpected"];
  console.log(`summary: ${order.filter((o) => counts.has(o)).map((o) => `${counts.get(o)} ${o}`).join(", ")}`);
  if (options.fuzz) console.log(`seed: ${options.seed}`);
  if (kept.length > 0) console.log(`kept ${kept.length} program${kept.length === 1 ? "" : "s"} in ${displayPath(options.out)}`);
  if (findings.length > 0) {
    console.log(`\nfindings (crash, fatal error, or unrecognised outcome):`);
    for (const finding of findings) console.log(`  ${finding}`);
    process.exitCode = 1;
  }
};

// `... | head` closes stdout early; stop quietly instead of throwing EPIPE,
// keeping a findings exit status already set.
process.stdout.on("error", (error: NodeJS.ErrnoException) => {
  if (error.code !== "EPIPE") throw error;
  killGroup(activeChild?.pid);
  removeWorkDir();
  process.exit(process.exitCode ?? 0);
});

main().catch((error: unknown) => {
  console.error(error);
  process.exit(2);
});
