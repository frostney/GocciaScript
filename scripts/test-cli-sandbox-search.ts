#!/usr/bin/env bun
/**
 * test-cli-sandbox-search.ts
 *
 * Sandbox filesystem search through GocciaRunner's sandbox mode: every case
 * in fixtures/sandbox-search/vectors.json (fs.globSync, and grep and
 * fuzzyFind from goccia:search) against its expected result, the callback
 * and async-iterator forms of fs.glob, timestamps left alone, and a
 * catastrophic pattern stopped by the RegExp step limit.
 */

import { deepStrictEqual } from "node:assert";
import { mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { RUNNER } from "./test-cli/binaries";
import { makeTmpFactory, clean } from "./test-cli/tmpdir";

const makeTmp = makeTmpFactory("goccia-search-");

type FileSpec = string | { base64: string };
type Vectors = {
  trees: Record<string, Record<string, FileSpec>>;
  glob: { name: string; tree: string; pattern: string | string[]; options?: object; expect: unknown }[];
  fuzzyFind: { name: string; tree: string; query: string; options?: object; expect: unknown }[];
  grep: { name: string; tree: string; pattern: string; options?: object; expect: unknown }[];
  errors: { name: string; tree: string; api: "glob" | "fuzzyFind" | "grep"; args: unknown[]; error: string }[];
};

const vectors: Vectors = JSON.parse(readFileSync("fixtures/sandbox-search/vectors.json", "utf8"));
const failures: string[] = [];

/**
 * Writes the tree and an entry script to the host, runs the entry in
 * sandbox mode and returns its stdout. The entry removes itself before it
 * searches, so the tree is exactly the vectors' tree.
 */
function runInTree(tree: Record<string, FileSpec>, body: string, extraArgs: string[] = []): string {
  const tmp = makeTmp();
  try {
    const root = join(tmp, "tree");
    for (const [path, spec] of Object.entries(tree)) {
      const target = join(root, ...path.split("/").filter(Boolean));
      mkdirSync(dirname(target), { recursive: true });
      writeFileSync(target, typeof spec === "string" ? spec : Buffer.from(spec.base64, "base64"));
    }
    const entry = "/__search_entry__.js";
    writeFileSync(
      join(root, entry),
      [
        'import fs from "fs";',
        'import { grep, fuzzyFind } from "goccia:search";',
        `fs.rmSync(${JSON.stringify(entry)});`,
        body,
      ].join("\n"),
    );
    const proc = Bun.spawnSync(
      [RUNNER, "--copy-rw", `${root}=/`, `--entry=${entry}`, "--source-type=module", ...extraArgs],
      { stdout: "pipe", stderr: "pipe" },
    );
    const stdout = proc.stdout.toString();
    if (proc.exitCode !== 0) throw new Error(`runner exited ${proc.exitCode}:\n${stdout}${proc.stderr.toString()}`);
    return stdout;
  } finally {
    clean(tmp);
  }
}

// -- the shared vectors --------------------------------------------------------

for (const treeName of Object.keys(vectors.trees)) {
  const cases = [
    ...vectors.glob.filter((c) => c.tree === treeName).map((c) => ({ kind: "glob", name: c.name, call: [c.pattern, c.options], expect: c.expect })),
    ...vectors.fuzzyFind.filter((c) => c.tree === treeName).map((c) => ({ kind: "fuzzyFind", name: c.name, call: [c.query, c.options], expect: c.expect })),
    ...vectors.grep.filter((c) => c.tree === treeName).map((c) => ({ kind: "grep", name: c.name, call: [c.pattern, c.options], expect: c.expect })),
    ...vectors.errors.filter((c) => c.tree === treeName).map((c) => ({ kind: c.api, name: c.name, call: c.args, error: c.error })),
  ];
  const body = [
    `const cases = ${JSON.stringify(cases.map((c) => ({ kind: c.kind, call: c.call })))};`,
    "const apis = { glob: (a, b) => fs.globSync(a, b), fuzzyFind: (a, b) => fuzzyFind(a, b), grep: (a, b) => grep(a, b) };",
    "const results = cases.map((c) => {",
    "  const args = c.call.filter((value) => value !== null);",
    "  try { return { value: apis[c.kind](...args) }; }",
    "  catch (error) { return { error: error.name, message: String(error.message) }; }",
    "});",
    "console.log(JSON.stringify(results));",
  ].join("\n");
  let results: { value?: unknown; error?: string; message?: string }[];
  try {
    results = JSON.parse(runInTree(vectors.trees[treeName], body).trim());
  } catch (error) {
    failures.push(`tree ${treeName}: ${(error as Error).message}`);
    continue;
  }
  cases.forEach((c, index) => {
    const got = results[index];
    try {
      if ("error" in c) {
        if (got.error !== c.error) throw new Error(`expected ${c.error}, got ${JSON.stringify(got)}`);
      } else {
        if (got.error !== undefined) throw new Error(`threw ${got.error}: ${got.message}`);
        deepStrictEqual(got.value, c.expect);
      }
    } catch (error) {
      failures.push(`${c.kind}: ${c.name}: ${(error as Error).message}`);
    }
  });
  console.log(`vectors (${treeName}): ${cases.length} cases`);
}

// -- the callback and async-iterator forms -------------------------------------

{
  const tree = vectors.trees.project;
  const body = [
    "const sync = fs.globSync(\"**/*.ts\");",
    "const viaCallback = await new Promise((resolve, reject) =>",
    "  fs.glob(\"**/*.ts\", (error, paths) => (error ? reject(error) : resolve(paths))));",
    "const viaOptionsCallback = await new Promise((resolve) => fs.glob(\"*.ts\", { cwd: \"/src\" }, (error, paths) => resolve(paths)));",
    "const iterated = [];",
    "for await (const path of fs.promises.glob(\"**/*.ts\")) iterated.push(path);",
    "const iterator = fs.promises.glob(\"src/*\");",
    "const first = await iterator.next();",
    "const closed = await iterator.return();",
    "const after = await iterator.next();",
    "let syncThrow = '';",
    "try { fs.glob(\"a/../b\", () => {}); } catch (error) { syncThrow = error.name; }",
    "console.log(JSON.stringify({ same: JSON.stringify(sync) === JSON.stringify(viaCallback) && JSON.stringify(sync) === JSON.stringify(iterated),",
    "  viaOptionsCallback, first, closed, after, syncThrow, hasAsyncIterator: typeof iterator[Symbol.asyncIterator] }));",
  ].join("\n");
  try {
    const got = JSON.parse(runInTree(tree, body).trim());
    deepStrictEqual(got, {
      same: true,
      viaOptionsCallback: ["index.ts"],
      first: { value: "src/app.tsx", done: false },
      closed: { done: true },
      after: { done: true },
      syncThrow: "TypeError",
      hasAsyncIterator: "function",
    });
    console.log("fs.glob and fs.promises.glob: ok");
  } catch (error) {
    failures.push(`fs.glob forms: ${(error as Error).message}`);
  }
}

// -- searching changes no timestamp --------------------------------------------

{
  const body = [
    "const paths = fs.globSync(\"**\", { cwd: \"/\" }).map((p) => \"/\" + p);",
    "const stamp = () => paths.map((p) => { const s = fs.statSync(p); return [p, s.atimeMs, s.mtimeMs, s.ctimeMs].join(); }).join(\"\\n\");",
    "const before = stamp();",
    "grep(\"export\", { context: 2 });",
    "fuzzyFind(\"ctrl\");",
    "fs.globSync(\"**/*\");",
    "console.log(stamp() === before ? \"unchanged\" : \"changed\");",
  ].join("\n");
  try {
    const out = runInTree(vectors.trees.project, body).trim();
    if (out !== "unchanged") throw new Error(out);
    console.log("timestamps: ok");
  } catch (error) {
    failures.push(`timestamps: ${(error as Error).message}`);
  }
}

// -- a catastrophic pattern stops at the step limit ----------------------------

{
  const body = [
    "fs.writeFileSync(\"/long.txt\", \"a\".repeat(30000) + \"!\\n\");",
    "try { grep(\"(a+)+$\", { regex: true, glob: \"long.txt\" }); console.log(\"returned\"); }",
    "catch (error) { console.log(error.name + \": \" + /step count exceeded/.test(error.message)); }",
  ].join("\n");
  try {
    const out = runInTree({ "/readme.md": "x\n" }, body, ["--timeout=20000"]).trim();
    if (out !== "Error: true") throw new Error(out);
    console.log("step limit: ok");
  } catch (error) {
    failures.push(`step limit: ${(error as Error).message}`);
  }
}

// -- adversarial inputs stay cheap ---------------------------------------------

{
  const body = [
    "fs.writeFileSync(\"/long.txt\", \"a\".repeat(400000) + \"\\n\");",
    "const started = Date.now();",
    "const literal = grep(\"a\".repeat(200000) + \"b\", { glob: \"long.txt\" }).matches.length;",
    "const fuzzy = fuzzyFind(\"q\".repeat(1000000)).matches.length;",
    "let glob = \"\";",
    "try { fs.globSync(\"{a,b}\".repeat(10) + \"x\".repeat(20000)); } catch (error) { glob = error.name; }",
    "let long = \"\";",
    "try { fs.globSync(\"x\".repeat(1000000)); } catch (error) { long = error.name; }",
    "console.log(JSON.stringify({ literal, fuzzy, glob, long }));",
  ].join("\n");
  try {
    const startedAt = Date.now();
    const out = JSON.parse(runInTree({ "/readme.md": "x\n" }, body, ["--timeout=30000"]).trim());
    deepStrictEqual(out, { literal: 0, fuzzy: 0, glob: "TypeError", long: "TypeError" });
    console.log(`adversarial inputs: ok (${Date.now() - startedAt} ms)`);
  } catch (error) {
    failures.push(`adversarial inputs: ${(error as Error).message}`);
  }
}

if (failures.length > 0) {
  console.error(`\n${failures.length} failure(s):`);
  for (const failure of failures) console.error(`  - ${failure}`);
  process.exit(1);
}
console.log("\nAll test-cli-sandbox-search.ts tests passed.");
