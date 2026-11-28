#!/usr/bin/env bun
/**
 * test-cli.ts
 *
 * Common CLI options tested across all apps: stdin smoke, --help, --allow-ffi,
 * --compat-asi, --source-type, .mjs source-type inference, --compat-var, --compat-loose-equality, --compat-non-strict-mode,
 * --compat-for-in-loop, --compat-while-loops, --compat-annex-b, --warning-unsupported-features,
 * --timeout, --max-instructions, --max-memory, --max-stack, --log,
 * example scripts.
 */

import { $ } from "bun";
import { verifyOutputEncoding, verifyReporterEncoding } from "./test-cli-output-encoding";
import {
  writeFileSync,
  readFileSync,
  existsSync,
  mkdirSync,
  symlinkSync,
  linkSync,
  realpathSync,
} from "fs";
import { join, resolve } from "path";
import {
  RUNNER,
  BARE,
  REPL,
  TESTRUNNER,
  TEST262RUNNER,
  BUNDLER,
  BENCHRUNNER,
} from "./test-cli/binaries";
import { canCapMemory, containsLine, normalizeLineEndings, runLoaderJson } from "./test-cli/assertions";
import { assertPeakRssBelow, runWithPeakRss } from "./test-cli/rss";
import { mkdtemp, clean } from "./test-cli/tmpdir";
import { verifyModuleIdentity } from "./test-cli/module-identity";

verifyOutputEncoding();
verifyReporterEncoding();
verifyModuleIdentity();

// -- Stdin smoke (Loader) ------------------------------------------------------

console.log("Stdin smoke...");
{
  const out = await $`echo 'const x = 2 + 2; x;' | ${RUNNER} --print -`.text();
  if (!containsLine(out, "4")) throw new Error(`Expected 4 on its own line, got: ${out}`);
}

console.log("Module top-level await keeps the last value and fails only on its own awaits...");
{
  // A module entry with top-level await is an async body (ES2026
  // §16.2.1.7.3.2 ExecuteModule step 10). Its last statement's value is still
  // the run's result, and is never adopted as if the module had awaited it.
  for (const [source, expected] of [
    ["await 0; 42;", 42],
    ["await 0; ({ answer: 42 });", { answer: 42 }],
    ["await 0; const p = Promise.reject('handled'); p.catch(() => {}); p;", {}],
    ["await 0; new Promise(() => {});", {}],
    ["await 0; ({ then() { console.log('then called'); } });", {}],
  ] as const) {
    const { exitCode, json, stderr } = runLoaderJson(source, ["--source-type=module"]);
    if (exitCode !== 0 || JSON.stringify(json.files?.[0]?.result) !== JSON.stringify(expected) ||
        JSON.stringify(json.output).includes("then called"))
      throw new Error(
        `Module ${JSON.stringify(source)} should exit 0 with result ${JSON.stringify(expected)}, ` +
          `got exit ${exitCode}, result ${JSON.stringify(json.files?.[0]?.result)}, output ${JSON.stringify(json.output)}${stderr}`,
      );
  }
  const printed = await $`printf %s 'await 0; 42;' | ${RUNNER} --source-type=module --print -`.text();
  if (!containsLine(printed, "42")) throw new Error(`Expected 42 on its own line, got: ${printed}`);
  const rejected = runLoaderJson("await 0; await Promise.reject(new TypeError('rejected'));", ["--source-type=module"]);
  if (rejected.exitCode === 0 || rejected.json.error?.type !== "TypeError")
    throw new Error(`A rejected top-level await should fail with its TypeError, got exit ${rejected.exitCode}, ${JSON.stringify(rejected.json.error)}`);
}

console.log("Module code releases what a top-level statement read; only an entry keeps a completion value...");
{
  // A sandbox entry runs through the engine's module evaluation, where the
  // module's bindings live in its environment record and a read copies a
  // binding's value into a register of the module's frame. Each top-level
  // statement clears the registers it used, so a value the module read and
  // dropped is free for the next allocation under the memory limit.
  const tmp = mkdtemp("goccia-module-top-level-");
  try {
    const size = "6 * 1024 * 1024";
    const allocateAgain =
      `try { const again = new ArrayBuffer(${size}); console.log("again ok", again.byteLength); } ` +
      `catch (error) { console.log("again refused", error.name); }`;
    const shapes: Record<string, string> = {
      "awaited.mjs": `const make = async () => new ArrayBuffer(${size});\nlet buffer = await make();\n` +
        `const length = buffer.byteLength;\nbuffer = null;\n${allocateAgain}\n`,
      "not-awaited.mjs": `let buffer = new ArrayBuffer(${size});\nconst length = buffer.byteLength;\n` +
        `buffer = null;\n${allocateAgain}\n`,
    };
    for (const [name, source] of Object.entries(shapes)) {
      writeFileSync(join(tmp, name), source);
      const proc = Bun.spawnSync([resolve(RUNNER), "--sandbox", "--max-memory=10MiB", name], {
        cwd: tmp,
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0 || !containsLine(out, "again ok 6291456"))
        throw new Error(`Sandbox module ${name} should allocate again after dropping its value, got exit ${proc.exitCode}: ${out}`);
    }

    // A script entry that imports its own file loads it as a separate module
    // (docs/engine/README.md, Script entries with imports), which keeps no
    // completion value either: its
    // expression statement's buffer is free for the allocation after it.
    writeFileSync(
      join(tmp, "self.js"),
      [
        "const first = !globalThis.selfImported;",
        "globalThis.selfImported = true;",
        'if (first) import("./self.js");',
        `first ? 0 : new ArrayBuffer(${size});`,
        `if (!first) { ${allocateAgain} }`,
      ].join("\n") + "\n",
    );
    const selfImport = Bun.spawnSync(
      [resolve(RUNNER), "--source-type=script", "--max-memory=10MiB", "self.js"],
      { cwd: tmp, stdout: "pipe", stderr: "pipe" },
    );
    const selfOut = selfImport.stdout.toString() + selfImport.stderr.toString();
    if (selfImport.exitCode !== 0 || !containsLine(selfOut, "again ok 6291456"))
      throw new Error(`A script entry's import of its own file should keep no completion value, got exit ${selfImport.exitCode}: ${selfOut}`);

    // An imported module reports no completion value; the entry's last
    // statement value is still the run's result, from the runner and from a
    // sandbox entry alike.
    writeFileSync(join(tmp, "dependency.mjs"), "export const one = 1;\n99;\n");
    writeFileSync(join(tmp, "entry.mjs"), 'import { one } from "./dependency.mjs";\n41 + one;\n');
    const runner = Bun.spawnSync([resolve(RUNNER), "--output=compact-json", "entry.mjs"], {
      cwd: tmp,
      stdout: "pipe",
      stderr: "pipe",
    });
    const result = JSON.parse(runner.stdout.toString()).files?.[0]?.result;
    if (runner.exitCode !== 0 || result !== 42)
      throw new Error(`An entry module's result should stay 42, got exit ${runner.exitCode}, result ${JSON.stringify(result)}: ${runner.stderr.toString()}`);
    const sandboxed = Bun.spawnSync(
      [resolve(RUNNER), "--sandbox", "--copy", "dependency.mjs", "--print", "entry.mjs"],
      { cwd: tmp, stdout: "pipe", stderr: "pipe" },
    );
    const printed = sandboxed.stdout.toString();
    if (sandboxed.exitCode !== 0 || !containsLine(printed, "42") || containsLine(printed, "99"))
      throw new Error(`A sandbox entry module should print 42, got exit ${sandboxed.exitCode}: ${printed}${sandboxed.stderr.toString()}`);
  } finally {
    clean(tmp);
  }
}

console.log("REPL stdin decodes UTF-8...");
{
  const repl = Bun.spawnSync([REPL], {
    stdin: new TextEncoder().encode('"😀".length;\n'),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = repl.stdout.toString() + repl.stderr.toString();
  const hasExpectedResult = normalizeLineEndings(output)
    .split("\n")
    .some((line) => line === "2" || line === "> 2");
  if (repl.exitCode !== 0 || !hasExpectedResult)
    throw new Error(`REPL UTF-8 stdin expected UTF-16 length 2, got: ${output}`);
}

console.log("Top-level lexical TDZ...");
for (const [label, source] of [
  ["let", "let x = x;\n"],
  ["const", "const x = x;\n"],
] as const) {
  const { exitCode, json } = runLoaderJson(source);
  if (exitCode === 0) throw new Error(`Top-level ${label} self-reference should fail`);
  if (json.error?.type !== "ReferenceError")
    throw new Error(`Top-level ${label} self-reference should throw ReferenceError, got ${json.error?.type}`);
}

console.log("Stack frames locate each caller at the call it is making...");
{
  // The docs/errors.md § Stack Traces sample. Each caller frame is located at
  // its own call to the next frame, and the top level at its call to outer.
  // Every position is the one V8 (Node) prints: a call at its callee's name,
  // and the failed read at the property name.
  const source = [
    "const inner = (obj) => {",
    "  return obj.x;",
    "};",
    "const middle = (obj) => {",
    "  inner(obj);",
    "};",
    "const outer = () => {",
    "  middle(null);",
    "};",
    "try {",
    "  outer();",
    "} catch (e) {",
    "  console.log(e.stack);",
    "}",
    "",
  ].join("\n");
  const proc = Bun.spawnSync([RUNNER], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  const out = proc.stdout.toString();
  for (const frame of [
    "    at inner (<stdin>:2:14)",
    "    at middle (<stdin>:5:3)",
    "    at outer (<stdin>:8:3)",
    "    at <module> (<stdin>:11:3)",
  ]) {
    if (!containsLine(out, frame))
      throw new Error(`Stack should contain "${frame}", got: ${out}`);
  }
}

console.log("A computed static field's initializer is named from its key...");
{
  // A static field initializer runs as its own method (ES2026 §15.7.10), and
  // an anonymous function or class in it still takes its name from the
  // computed key (§8.4.5 NamedEvaluation).
  const source = [
    "const key = Symbol('sym');",
    "class Base { static greet() { return 'base'; } }",
    "class C extends Base {",
    "  static ['arrow'] = () => super.greet();",
    "  static [key] = () => 'fn';",
    "  static ['klass'] = class { static ownName = this.name; };",
    "}",
    "console.log([C.arrow.name, C.arrow(), C[key].name, C.klass.name, C.klass.ownName].join(','));",
    "const E = class extends Base {",
    "  static ['arrow'] = () => super.greet();",
    "  static [key] = () => 'fn';",
    "  static ['klass'] = class { static ownName = this.name; };",
    "};",
    "console.log([E.arrow.name, E.arrow(), E[key].name, E.klass.name, E.klass.ownName].join(','));",
    "",
  ].join("\n");
  const proc = Bun.spawnSync([RUNNER], {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  });
  // (A field initializer is parsed with neither [Yield] nor [Await], §15.7.1,
  // so a yield there is a SyntaxError rather than a suspension to name.)
  const out = proc.stdout.toString().trim().split(/\r?\n/).slice(0, 2).join("|");
  const expected = "arrow,base,[sym],klass,klass|arrow,base,[sym],klass,klass";
  if (proc.exitCode !== 0 || out !== expected)
    throw new Error(`Computed static field names expected ${expected}, got: ${proc.stdout.toString()}${proc.stderr.toString()}`);
}

console.log("--output=json reports where a thrown error was created...");
{
  const { exitCode, json } = runLoaderJson(
    'const fail = () => {\n  throw new TypeError("boom");\n};\nfail();\n',
  );
  if (exitCode === 0) throw new Error(`A thrown TypeError should fail the run`);
  if (json.error?.line !== 2 || json.error?.column !== 9)
    throw new Error(
      `JSON error should be located at 2:9, got ${json.error?.line}:${json.error?.column}`,
    );
}

// The `-->` header of a diagnostic, and the column of the caret in the code
// frame under it, counted from the first character after the gutter's "| ".
const codeFrameLocation = (out: string): { header: string; caretColumn: number } => {
  const lines = normalizeLineEndings(out).split("\n");
  const at = lines.findIndex((line) => line.trimStart().startsWith("--> "));
  if (at < 0) return { header: "", caretColumn: 0 };
  const caretLine = lines.slice(at + 1).find((line) => /^\s*\|\s*\^/.test(line));
  const caretColumn = caretLine
    ? caretLine.indexOf("^") - caretLine.indexOf("|") - 1
    : 0;
  return { header: lines[at].trim(), caretColumn };
};

console.log("An uncaught failed property read is located at its property name...");
{
  const tmp = mkdtemp("goccia-member-read-");
  try {
    const file = join(tmp, "member.js");
    // Node reports `obj.x` at `x`, `holder.a.b` at `b`, the property whose read
    // failed, and a computed read at its `[`.
    const cases = [
      ["const inner = (obj) => {\n  return obj.x;\n};\ninner(null);\n", 2, 14],
      ["const holder = { a: null };\nconst read = () => holder.a.b;\nread();\n", 2, 29],
      ['const holder = { a: null };\nconst read = () => holder.a["b"];\nread();\n', 2, 28],
    ] as const;
    for (const [source, line, column] of cases) {
      writeFileSync(file, source);
      const proc = Bun.spawnSync([RUNNER, file], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      const { header, caretColumn } = codeFrameLocation(out);
      if (proc.exitCode === 0 || header !== `--> ${file}:${line}:${column}` || caretColumn !== column)
        throw new Error(
          `Failed read should be located at ${line}:${column} with the caret under it, got header "${header}", caret ${caretColumn}: ${out}`,
        );
    }
  } finally {
    clean(tmp);
  }
}

console.log("A failed strict-types parameter check is located at its parameter...");
{
  const tmp = mkdtemp("goccia-strict-parameter-");
  try {
    const file = join(tmp, "param.ts");
    const cases = [
      ['const f = (x: number) => x;\nf("a");\n', 1, 12],
      ['const f = (a: string,\n  x: number) => x;\nf("s", "a");\n', 2, 3],
      ['const o = {\n  set v(x: number) {},\n};\no.v = "a";\n', 2, 9],
    ] as const;
    for (const [source, line, column] of cases) {
      writeFileSync(file, source);
      const proc = Bun.spawnSync([RUNNER, file, "--strict-types"], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      const { header, caretColumn } = codeFrameLocation(out);
      if (proc.exitCode === 0 || header !== `--> ${file}:${line}:${column}` || caretColumn !== column)
        throw new Error(
          `A failed parameter check should be located at ${line}:${column} with the caret under it, got header "${header}", caret ${caretColumn}: ${out}`,
        );
    }
    // An indexed access type is the type of the property it names.
    writeFileSync(
      file,
      'type N = { kind: "text" | "row" };\nconst f = (kind: N["kind"]) => kind;\nconsole.log(f("text"));\n',
    );
    const indexed = Bun.spawnSync([RUNNER, file, "--strict-types"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const indexedOut = indexed.stdout.toString() + indexed.stderr.toString();
    if (indexed.exitCode !== 0 || !indexedOut.includes("text"))
      throw new Error(`An indexed access parameter should accept "text", got: ${indexedOut}`);
  } finally {
    clean(tmp);
  }
}

console.log("A failed import() is located at the import() call...");
{
  const tmp = mkdtemp("goccia-dynamic-import-missing-");
  try {
    const topLevel = join(tmp, "dyn.js");
    writeFileSync(topLevel, 'const x = 1;\nimport("./nope.js");\n');
    const inAsync = join(tmp, "dyn-async.js");
    writeFileSync(
      inAsync,
      'const load = async () => {\n  await import("./nope.js");\n};\nload();\n',
    );
    const cases = [
      [topLevel, 2, 1, '2 | import("./nope.js");'],
      [inAsync, 2, 9, '2 |   await import("./nope.js");'],
    ] as const;
    for (const [file, line, column, excerpt] of cases) {
      const proc = Bun.spawnSync([RUNNER, file], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      const { header, caretColumn } = codeFrameLocation(out);
      if (
        proc.exitCode === 0 ||
        !out.includes('Module not found: "./nope.js"') ||
        header !== `--> ${file}:${line}:${column}` ||
        caretColumn !== column ||
        !out.includes(excerpt)
      )
        throw new Error(
          `A failed import() should be located at ${line}:${column} with a code frame, got: ${out}`,
        );
      const json = Bun.spawnSync([RUNNER, file, "--output=json"], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const error = JSON.parse(json.stdout.toString()).error;
      if (error?.line !== line || error?.column !== column)
        throw new Error(
          `A failed import() should report ${line}:${column} in JSON, got ${error?.line}:${error?.column}`,
        );
    }
  } finally {
    clean(tmp);
  }
}

console.log("A static import of a missing module is located at its declaration...");
{
  const tmp = mkdtemp("goccia-static-import-missing-");
  try {
    const entry = join(tmp, "entry.js");
    writeFileSync(entry, 'const y = 2;\nimport { a } from "./missing.js";\nconsole.log(a);\n');
    // A module the entry imports fails the same way, located in its own file.
    const viaDep = join(tmp, "via-dep.js");
    writeFileSync(viaDep, 'import { b } from "./dep.js";\nconsole.log(b);\n');
    const dep = join(tmp, "dep.js");
    writeFileSync(dep, '// dep\nexport { a as b } from "./missing.js";\n');
    const cases = [
      [entry, `--> ${entry}:2:1`],
      // Module requests resolve from the canonical referrer directory. On Windows
      // the native realpath expands the 8.3 components that tmpdir() may use.
      [viaDep, `--> ${realpathSync.native(dep)}:2:1`],
    ] as const;
    for (const [file, expected] of cases) {
      const proc = Bun.spawnSync([RUNNER, file], {
        stdout: "pipe",
        stderr: "pipe",
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      const { header } = codeFrameLocation(out);
      if (
        proc.exitCode === 0 ||
        !out.includes('Module not found: "./missing.js"') ||
        !out.includes("Resolved to: ") ||
        header !== expected
      )
        throw new Error(
          `A missing static import should be located at "${expected}", got: ${out}`,
        );
    }
  } finally {
    clean(tmp);
  }
}

console.log("--output=json names an uncaught private-member error's member as written...");
for (const [label, source, message] of [
  [
    "field",
    "class C {\n  #p = 1;\n  static read(o) { return o.#p; }\n}\nC.read({});\n",
    "Private field #p is not accessible",
  ],
  [
    "method",
    "class C {\n  #m() {}\n  static write(o) { o.#m = 1; }\n}\nC.write(new C());\n",
    "Private method #m is not writable",
  ],
] as const) {
  const { exitCode, json } = runLoaderJson(source);
  if (exitCode === 0) throw new Error(`An uncaught private ${label} error should fail the run`);
  if (json.error?.type !== "TypeError" || json.error?.message !== message)
    throw new Error(
      `JSON private ${label} error should be TypeError "${message}", got ${json.error?.type} "${json.error?.message}"`,
    );
}

console.log("Top-level const with a mismatched strict type...");
{
  const { exitCode, json } = runLoaderJson(
    "const limit: string = 16;\nconst read = () => limit;\nread();\n",
    ["--strict-types"],
  );
  if (exitCode === 0) throw new Error(`Mismatched top-level const type should fail`);
  if (json.error?.type !== "TypeError")
    throw new Error(`Mismatched top-level const type should throw TypeError, got ${json.error?.type}`);
}

console.log("REPL top-level const across inputs...");
{
  const repl = Bun.spawnSync([REPL], {
    stdin: new TextEncoder().encode(
      [
        "const limit = 16; const readSameInput = () => limit;",
        "const readLaterInput = () => limit + 1;",
        "const limit = 17;",
        "[readSameInput(), readLaterInput(), limit].join(\",\");",
        "",
      ].join("\n"),
    ),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = normalizeLineEndings(repl.stdout.toString() + repl.stderr.toString());
  if (repl.exitCode !== 0 || !output.includes("16,17,16"))
    throw new Error(`REPL top-level const across inputs expected 16,17,16, got: ${output}`);
  if (!output.includes("SyntaxError"))
    throw new Error(`REPL should reject redeclaring a top-level const, got: ${output}`);
}

console.log("REPL strict-types checks writes to typed top-level bindings from later inputs...");
{
  const repl = Bun.spawnSync([REPL, "--strict-types", "--compat-var", "--compat-function"], {
    stdin: new TextEncoder().encode(
      [
        'let text: string = "a"; var count: number = 1;',
        'try { text = 5; } catch (e) { console.log("let:" + e.name); }',
        'try { count = "x"; } catch (e) { console.log("var:" + e.name); }',
        'try { globalThis.count = "x"; } catch (e) { console.log("prop:" + e.name); }',
        'var count: string = "y";',
        'function count() {}',
        'globalThis.loose = 1;',
        'var loose: number = 2;',
        'delete globalThis.loose; globalThis.loose = "free"; console.log("loose:" + loose);',
        'text = "b"; count = 2; console.log("kept:" + text + count);',
        'let held = 1; Object.defineProperty(globalThis, "viaSetter", { configurable: true, get() { return held; }, set(v) { held = v; } });',
        'var viaSetter: number = 2;',
        'try { viaSetter = "x"; } catch (e) { console.log("setter:" + e.name); }',
        'try { Reflect.set(globalThis, "viaSetter", "x"); } catch (e) { console.log("reflect-setter:" + e.name + held); }',
        "",
      ].join("\n"),
    ),
    stdout: "pipe",
    stderr: "pipe",
  });
  const output = normalizeLineEndings(repl.stdout.toString() + repl.stderr.toString());
  for (const expected of ["let:TypeError", "var:TypeError", "prop:TypeError", "loose:free", "kept:b2", "setter:TypeError", "reflect-setter:TypeError2"])
    if (!output.includes(expected))
      throw new Error(`REPL strict-types across inputs expected ${expected}, got: ${output}`);
  // a later var of the name keeps the first type (its initializer is checked against it), and a function
  // declaration that would replace a number-typed var is refused before it is bound
  const typeErrors = output.split("\n").filter((line) => line.includes("Type 'string' is not assignable to type 'number'"));
  const functionErrors = output.split("\n").filter((line) => line.includes("Type 'function' is not assignable to type 'number'"));
  if (typeErrors.length < 1 || functionErrors.length !== 1)
    throw new Error(`REPL strict-types redeclarations expected TypeErrors, got: ${output}`);
}

console.log("REPL import bindings across inputs...");
{
  const tmp = mkdtemp("goccia-repl-import-");
  try {
    writeFileSync(
      join(tmp, "dep.mjs"),
      [
        "export const answer = 42;",
        "export let count = 0;",
        "export const increment = () => { count = count + 1; };",
        'export default "fallback";',
        "",
      ].join("\n"),
    );
    const repl = Bun.spawnSync([resolve(REPL)], {
      cwd: tmp,
      stdin: new TextEncoder().encode(
        [
          'import { answer, count, increment } from "./dep.mjs";',
          'import { answer as renamed } from "./dep.mjs";',
          'import fallback from "./dep.mjs";',
          'import * as ns from "./dep.mjs";',
          '[answer, renamed, fallback, ns.answer].join(",");',
          "increment();",
          '["live", count, ns.count].join(":");',
          "answer = 1;",
          "const answer = 2;",
          'import { count as renamed } from "./dep.mjs";',
          '["kept", answer, renamed].join(":");',
          "",
        ].join("\n"),
      ),
      stdout: "pipe",
      stderr: "pipe",
    });
    const output = normalizeLineEndings(repl.stdout.toString() + repl.stderr.toString());
    if (repl.exitCode !== 0 || !output.includes("42,42,fallback,42"))
      throw new Error(`REPL imports across inputs expected 42,42,fallback,42, got: ${output}`);
    if (!output.includes("live:1:1"))
      throw new Error(`REPL import binding should stay live across inputs, got: ${output}`);
    if (!output.includes("TypeError: Assignment to constant variable 'answer'"))
      throw new Error(`REPL should reject assigning to an import binding, got: ${output}`);
    if (!output.includes("SyntaxError: Identifier 'answer' has already been declared"))
      throw new Error(`REPL should reject redeclaring an imported name, got: ${output}`);
    if (!output.includes("SyntaxError: Identifier 'renamed' has already been declared"))
      throw new Error(`REPL should reject importing an already imported name again, got: ${output}`);
    if (!output.includes("kept:42:42"))
      throw new Error(`REPL import binding should survive the rejected inputs, got: ${output}`);

    // A missing export is rejected before the input runs, so the name is
    // never declared and a corrected import of it succeeds (#1274).
    const retry = Bun.spawnSync([resolve(REPL)], {
      cwd: tmp,
      stdin: new TextEncoder().encode(
        [
          'import { missing as answer } from "./dep.mjs";',
          'import { answer } from "./dep.mjs";',
          '["retried", answer].join(":");',
          "",
        ].join("\n"),
      ),
      stdout: "pipe",
      stderr: "pipe",
    });
    const retryOutput = normalizeLineEndings(retry.stdout.toString() + retry.stderr.toString());
    if (!retryOutput.includes('has no export named "missing"'))
      throw new Error(`REPL should reject a missing export, got: ${retryOutput}`);
    if (retry.exitCode !== 0 || !retryOutput.includes("retried:42"))
      throw new Error(`REPL should accept a corrected import after a missing export, got: ${retryOutput}`);
  } finally {
    clean(tmp);
  }
}

// -- Stdin smoke (TestRunner) --------------------------------------------------

console.log("Stdin smoke (TestRunner)...");
{
  const src = `test("two plus two", () => { expect(2 + 2).toBe(4); });\n`;

  // No positional argument -> stdin
  const out = await $`echo ${src} | ${TESTRUNNER} --no-progress`.text();
  if (!out.includes("Passed: 1")) throw new Error(`TestRunner stdin (no arg) expected Passed: 1, got: ${out}`);

  // Sole "-" arg -> stdin
  const outDash = await $`echo ${src} | ${TESTRUNNER} - --no-progress`.text();
  if (!outDash.includes("Passed: 1")) throw new Error(`TestRunner stdin ("-" arg) expected Passed: 1, got: ${outDash}`);
}

// -- Stdin smoke (BenchmarkRunner) ---------------------------------------------

console.log("Stdin smoke (BenchmarkRunner)...");
{
  const src = `import { bench, group } from "goccia:microbench"; group("stdin", () => { bench("sum", () => 1 + 1); });\n`;
  const out = await $`echo ${src} | ${BENCHRUNNER} --source-type=module --no-progress 2>&1`.text();
  if (!out.includes("sum")) throw new Error(`BenchmarkRunner stdin expected "sum" benchmark, got: ${out}`);

  const outDash = await $`echo ${src} | ${BENCHRUNNER} - --source-type=module --no-progress 2>&1`.text();
  if (!outDash.includes("sum")) throw new Error(`BenchmarkRunner stdin ("-" arg) expected "sum" benchmark, got: ${outDash}`);
}

// -- Stdin mixed-with-paths rejection (all three runners) ----------------------

console.log("Stdin mixed-with-paths rejection (Loader, TestRunner, BenchmarkRunner)...");
{
  const tmp = mkdtemp("goccia-stdin-mix-");
  try {
    const f = join(tmp, "x.js");
    writeFileSync(f, "1;\n");

    for (const [bin, label] of [[RUNNER, "Loader"], [TESTRUNNER, "TestRunner"], [BENCHRUNNER, "BenchmarkRunner"]] as const) {
      const proc = await $`${bin} - ${f} 2>&1`.nothrow();
      if (proc.exitCode === 0) throw new Error(`${label} should reject "-" mixed with file paths`);
      if (!proc.text().includes("stdin is supported only as the sole input"))
        throw new Error(`${label} mixed-path error missing unified message, got: ${proc.text()}`);
    }
  } finally {
    clean(tmp);
  }
}

// -- --help (all 6 apps) -------------------------------------------------------

console.log("--help (all 6 apps)...");
for (const bin of [RUNNER, BARE, REPL, TESTRUNNER, BUNDLER, BENCHRUNNER]) {
  const help = await $`${bin} --help 2>&1`.text();
  if (!help.includes("--")) throw new Error(`${bin} --help missing options`);
  if (!help.includes("--warning-unsupported-features"))
    throw new Error(`${bin} --help missing --warning-unsupported-features`);
  if (!help.includes("--deterministic"))
    throw new Error(`${bin} --help missing --deterministic`);
}

// -- --deterministic -----------------------------------------------------------

console.log("--deterministic (Loader + Bare)...");
{
  const source = [
    "Math.random()",
    "Math.random()",
    "Date.now()",
    "new Date().getTime()",
    "new Date(0).getHours()",
    "Temporal.Now.instant().epochNanoseconds.toString()",
    "Temporal.Now.timeZoneId()",
    "performance.timeOrigin",
    "performance.now()",
    'new Intl.DateTimeFormat("en", { year: "numeric" }).format()',
    'new Intl.DateTimeFormat("en").resolvedOptions().timeZone',
  ].join(", ");
  const expected =
    "0.8833108082136426|0.43152799704850997|0|0|0|0|UTC|0|0|1970|UTC";

  for (let run = 0; run < 2; run++) {
    const { exitCode, json, stderr } = runLoaderJson(
      `[${source}].join("|");\n`,
      ["--deterministic"],
    );
    if (exitCode !== 0)
      throw new Error(`Loader deterministic failed: ${stderr}`);
    if (json.files?.[0]?.result !== expected)
      throw new Error(
        `Loader deterministic run ${run + 1} expected ${expected}, got ${json.files?.[0]?.result}`,
      );
  }

  const bareSource =
    '[Math.random(), Math.random(), Date.now(), Temporal.Now.instant().epochNanoseconds.toString(), Temporal.Now.timeZoneId()].join("|");\n';
  const bare = Bun.spawnSync(
    [BARE, "-", "--print", "--deterministic"],
    {
      stdin: new TextEncoder().encode(bareSource),
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  const bareExpected =
    "0.8833108082136426|0.43152799704850997|0|0|UTC";
  if (bare.exitCode !== 0 || bare.stdout.toString().trim() !== bareExpected)
    throw new Error(
      `Bare deterministic expected ${bareExpected}, got ${bare.stdout.toString()}${bare.stderr.toString()}`,
    );
}

console.log("--deterministic child realms use stable distinct streams...");
{
  const shadow = runLoaderJson(
    'const child = new ShadowRealm(); [Math.random(), child.evaluate("Math.random()")].join("|");\n',
    ["--deterministic", "--unsafe-shadowrealm"],
  );
  const shadowExpected = "0.8833108082136426|0.6524484863740322";
  if (shadow.exitCode !== 0 || shadow.json.files?.[0]?.result !== shadowExpected)
    throw new Error(
      `ShadowRealm deterministic expected ${shadowExpected}, got ${shadow.json.files?.[0]?.result}${shadow.stderr}`,
    );

  const bareRealmSource = [
    "const realm = $262.createRealm();",
    'print([Math.random(), realm.evalScript("Math.random()"), realm.evalScript("$262.createRealm().evalScript(\\"Math.random()\\")")].join("|"));',
    "",
  ].join("\n");
  const bareRealm = Bun.spawnSync(
    [
      TEST262RUNNER,
      "--eval-host",
      "--deterministic",
    ],
    {
      stdin: new TextEncoder().encode(bareRealmSource),
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  const bareRealmExpected =
    "0.8833108082136426|0.11260056966858045|0.40713339556004036";
  if (
    bareRealm.exitCode !== 0 ||
    bareRealm.stdout.toString().trim() !== bareRealmExpected
  )
    throw new Error(
      `Test262 realm deterministic expected ${bareRealmExpected}, got ${bareRealm.stdout.toString()}${bareRealm.stderr.toString()}`,
    );
}

console.log("--deterministic keeps timeout clock live...");
{
  const proc = Bun.spawnSync(
    [RUNNER, "--deterministic", "--compat-while-loops", "--timeout=20"],
    {
      stdin: new TextEncoder().encode("while (true) {}\n"),
      stdout: "pipe",
      stderr: "pipe",
      timeout: 5_000,
    },
  );
  const output = proc.stdout.toString() + proc.stderr.toString();
  if (proc.exitCode === 0 || !output.includes("timed out"))
    throw new Error(
      `Deterministic timeout should use the real infrastructure clock, got exit ${proc.exitCode}: ${output}`,
    );
}

// -- --allow-ffi gating ---------------------------------------------------------

console.log("--allow-ffi gating...");
{
  const { json } = runLoaderJson("typeof FFI;\n");
  if (json.files?.[0]?.result !== "undefined") throw new Error(`FFI without a grant should be "undefined", got ${json.files?.[0]?.result}`);

  const { json: jsonOn } = runLoaderJson("typeof FFI;\n", ["--allow-ffi"]);
  if (jsonOn.files?.[0]?.result !== "object") throw new Error(`FFI with --allow-ffi should be "object", got ${jsonOn.files?.[0]?.result}`);

  const { json: jsonDenied } = runLoaderJson("typeof FFI;\n", ["--allow-ffi", "--deny-ffi"]);
  if (jsonDenied.files?.[0]?.result !== "undefined") throw new Error(`FFI with --deny-ffi should be "undefined", got ${jsonDenied.files?.[0]?.result}`);
}

// -- --compat-asi (Loader + Bundler) ---------------------------------------------------

console.log("--compat-asi (Loader + Bundler)...");
{
  const tmp = mkdtemp("goccia-asi-");
  try {
    const src = join(tmp, "no-semi.js");
    writeFileSync(src, "const x = 42\nx\n");

    // Loader without --compat-asi should fail
    const noAsi = await $`${RUNNER} ${src} 2>&1`.nothrow();
    if (noAsi.exitCode === 0) throw new Error("Loader should reject without --compat-asi");
    if (!noAsi.text().includes("SyntaxError")) throw new Error("Expected SyntaxError without --compat-asi");

    const oldAsi = await $`${RUNNER} ${src} --asi 2>&1`.nothrow();
    if (oldAsi.exitCode === 0) throw new Error("Loader should reject removed --asi alias");
    if (!oldAsi.text().includes("Unknown option: --asi")) throw new Error(`Expected unknown --asi, got: ${oldAsi.text()}`);

    // Loader with --compat-asi should succeed
    const withAsi = await $`${RUNNER} --print ${src} --compat-asi 2>&1`.text();
    if (!containsLine(withAsi, "42")) throw new Error(`Expected 42 with --compat-asi, got: ${withAsi}`);

    // Bundler without --compat-asi should fail
    const bundleNoAsi = await $`${BUNDLER} ${src} 2>&1`.nothrow();
    if (bundleNoAsi.exitCode === 0) throw new Error("Bundler should reject without --compat-asi");

    const bundleOldAsi = await $`${BUNDLER} ${src} --asi 2>&1`.nothrow();
    if (bundleOldAsi.exitCode === 0) throw new Error("Bundler should reject removed --asi alias");
    if (!bundleOldAsi.text().includes("Unknown option: --asi")) throw new Error(`Expected unknown --asi, got: ${bundleOldAsi.text()}`);

    // Bundler with --compat-asi should succeed
    await $`${BUNDLER} ${src} --compat-asi`.quiet();
    if (!existsSync(join(tmp, "no-semi.gbc"))) throw new Error("Bundler --compat-asi should produce .gbc");
  } finally {
    clean(tmp);
  }
}

// -- --compat-var (Loader + Bundler + TestRunner) --------------------------------

console.log("--compat-var (Loader + Bundler + TestRunner)...");
{
  const tmp = mkdtemp("goccia-var-");
  try {
    const src = join(tmp, "use-var.js");
    writeFileSync(src, "var x = 10;\nx;\n");

    // Loader with --compat-var
    const loaderOut = await $`${RUNNER} --print ${src} --compat-var 2>&1`.text();
    if (!containsLine(loaderOut, "10")) throw new Error(`Loader --compat-var expected 10, got: ${loaderOut}`);

    // Bundler with --compat-var
    await $`${BUNDLER} ${src} --compat-var`.quiet();
    if (!existsSync(join(tmp, "use-var.gbc"))) throw new Error("Bundler --compat-var should produce .gbc");

    // TestRunner with --compat-var
    const testSrc = join(tmp, "test-var.js");
    writeFileSync(
      testSrc,
      [
        "var y = 20;",
        'describe("var", () => {',
        '  test("works", () => {',
        "    expect(y).toBe(20);",
        "  });",
        "});",
      ].join("\n") + "\n",
    );
    const trOut = await $`${TESTRUNNER} ${testSrc} --compat-var --no-progress 2>&1`.text();
    if (!trOut.includes("Passed: 1")) throw new Error(`TestRunner --compat-var expected Passed: 1, got: ${trOut}`);
  } finally {
    clean(tmp);
  }
}

// -- --compat-function + Bare loader compat parsing ----------------------------

console.log("--compat-function (Loader) + Bare loader compat parsing...");
{
  const tmp = mkdtemp("goccia-func-");
  try {
    const fnSrc = join(tmp, "use-fn.js");
    writeFileSync(fnSrc, "function f() { return 7; }\nf();\n");
    const loaderOut = await $`${RUNNER} --print ${fnSrc} --compat-function 2>&1`.text();
    if (!containsLine(loaderOut, "7")) throw new Error(`Loader --compat-function expected 7, got: ${loaderOut}`);

    // Bare loader argv path — covered here so the full test262 suite isn't the
    // only thing exercising it.  Flag combo mirrors run_test262_suite.ts.
    const bothSrc = join(tmp, "use-both.js");
    writeFileSync(bothSrc, "var x = 22;\nfunction f() { return x; }\nf();\n");
    const bareOut = await $`${BARE} --print ${bothSrc} --compat-var --compat-function 2>&1`.text();
    if (bareOut.trim() !== "22") throw new Error(`Bare --compat-var --compat-function expected 22, got: ${bareOut}`);
    const bareNoFlag = await $`${BARE} ${bothSrc} 2>&1`.nothrow();
    if (bareNoFlag.exitCode === 0) throw new Error("Bare without compat flags should reject var/function");
    if (!bareNoFlag.text().includes("SyntaxError"))
      throw new Error(`Bare without compat flags should report SyntaxError, got: ${bareNoFlag.text()}`);

    // Stdin path — the exact shape run_test262_suite.ts uses (source piped
    // into a `-` argument).  Without this, file-path is the only invocation
    // mode covered.
    const bareStdin = await $`cat ${bothSrc} | ${BARE} --print - --compat-var --compat-function 2>&1`.text();
    if (bareStdin.trim() !== "22") throw new Error(`Bare stdin --compat-var --compat-function expected 22, got: ${bareStdin}`);

    const largeGlobalSrc = join(tmp, "large-global-declarations.js");
    writeFileSync(
      largeGlobalSrc,
      [
        `var ${Array.from({ length: 260 }, (_, i) => `globalName${i}`).join(", ")};`,
        "var lateVarBinding = 4;",
        "const lateConstBinding = 1;",
        "let lateLetBinding = 2;",
        "function lateFunctionBinding() { return 3; }",
        "lateVarBinding + lateConstBinding + lateLetBinding + lateFunctionBinding();",
        "",
      ].join("\n"),
    );
    const largeGlobalOut = await $`${BARE} --print ${largeGlobalSrc} --compat-var --compat-function 2>&1`.text();
    if (largeGlobalOut.trim() !== "10")
      throw new Error(`Bare large global declarations expected 10, got: ${largeGlobalOut}`);

    const wideNames = Array.from({ length: 260 }, (_, i) => `wideName${i}`);
    const wideDirectEvalSrc = join(tmp, "wide-direct-eval.js");
    writeFileSync(
      wideDirectEvalSrc,
      [
        "function readWideLocal() {",
        ...wideNames.map((name, index) => `  let ${name} = ${index};`),
        '  return eval("wideName259");',
        "}",
        "print(readWideLocal());",
        "",
      ].join("\n"),
    );
    const wideDirectEval = Bun.spawnSync(
      [TEST262RUNNER, "--eval-host"],
      {
        stdin: readFileSync(wideDirectEvalSrc),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const wideDirectEvalOut =
      wideDirectEval.stdout.toString() + wideDirectEval.stderr.toString();
    if (wideDirectEval.exitCode !== 0 || wideDirectEval.stdout.toString().trim() !== "259")
      throw new Error(`Test262 Runner wide direct eval expected
 259, got: ${wideDirectEvalOut}`);

    // A sloppy direct eval declares a function-level var that shadows a
    // top-level const for later reads in that function and its closures; a
    // strict one keeps its var to itself.
    const evalShadowSource = [
      "const SHADOWED = 16;",
      'const shadowedByVar = function () { eval("var SHADOWED = 5"); return SHADOWED; };',
      'const shadowedInClosure = function () { eval("var SHADOWED = 7"); return (() => SHADOWED)(); };',
      'const readByEval = function () { return eval("SHADOWED"); };',
      'const strictEval = function () { "use strict"; eval("var SHADOWED = 9"); return SHADOWED; };',
      'print([shadowedByVar(), shadowedInClosure(), readByEval(), strictEval()].join(","));',
      "",
    ].join("\n");
    const evalShadow = Bun.spawnSync(
      [TEST262RUNNER, "--eval-host"],
      {
        stdin: new TextEncoder().encode(evalShadowSource),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const evalShadowOut =
      evalShadow.stdout.toString() + evalShadow.stderr.toString();
    if (evalShadow.exitCode !== 0 || evalShadow.stdout.toString().trim() !== "5,7,16,16")
      throw new Error(`Test262 Runner direct eval shadowing a top-level const expected 5,7,16,16, got: ${evalShadowOut}`);

    // An assignment resolves its target before the right-hand side runs
    // (ES2026 §13.15.2), so a same-named var that a direct eval declares
    // there does not redirect it: a const still in its dead zone throws the
    // ReferenceError, an initialized one the TypeError (§9.1.1.1.5 step 3).
    const evalConstTargetSource = [
      "const outcome = function (run) {",
      "  try { run(); return 'none'; } catch (e) { return e.constructor.name + ': ' + e.message; }",
      "};",
      "const results = [",
      "  outcome(function () {",
      '    const inner = function () { early = eval("var early = 5; 1"); };',
      "    inner();",
      "    const early = 0;",
      "  }),",
      "  outcome(function () {",
      '    const inner = function () { TOP_LATE = eval("var TOP_LATE = 5; 1"); };',
      "    inner();",
      "  }),",
      "  outcome(function () {",
      "    const fixed = 0;",
      '    const inner = function () { fixed = eval("var fixed = 5; 1"); };',
      "    inner();",
      "  }),",
      "];",
      "const TOP_LATE = 1;",
      'print(results.join(" | "));',
      "",
    ].join("\n");
    const evalConstTargetExpected = [
      "ReferenceError: Cannot access 'early' before initialization",
      "ReferenceError: Cannot access 'TOP_LATE' before initialization",
      "TypeError: Assignment to constant variable 'fixed'",
    ].join(" | ");
    const evalConstTarget = Bun.spawnSync(
      [TEST262RUNNER, "--eval-host"],
      {
        stdin: new TextEncoder().encode(evalConstTargetSource),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const evalConstTargetOut =
      evalConstTarget.stdout.toString() + evalConstTarget.stderr.toString();
    if (evalConstTarget.exitCode !== 0 || evalConstTarget.stdout.toString().trim() !== evalConstTargetExpected)
      throw new Error(`Test262 Runner const assignment target across direct eval expected ${evalConstTargetExpected}, got: ${evalConstTargetOut}`);

    // A direct eval in an arrow sees the private names of the class body the
    // arrow is in, even when the arrow has no home object of its own, as in a
    // static field's array literal (ES2026 §19.2.1.1 PerformEval step 6.b).
    const evalPrivateArrowSource = [
      "const outcome = (run) => { try { return String(run()); } catch (e) { return e.name; } };",
      "class C { #x = 1; static read = () => eval('(new C()).#x'); }",
      "class I { static #u = 8; static arr = [() => eval('I.#u')]; }",
      "class K { static #k = 1; static arr = [() => eval('K.#missing')]; }",
      "const outside = [() => eval('({}).#a')];",
      "print([outcome(C.read), outcome(I.arr[0]), outcome(K.arr[0]), outcome(outside[0])].join(','));",
      "",
    ].join("\n");
    const evalPrivateArrow = Bun.spawnSync(
      [TEST262RUNNER, "--eval-host"],
      {
        stdin: new TextEncoder().encode(evalPrivateArrowSource),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const evalPrivateArrowOut =
      evalPrivateArrow.stdout.toString() + evalPrivateArrow.stderr.toString();
    if (evalPrivateArrow.exitCode !== 0 || evalPrivateArrow.stdout.toString().trim() !== "1,8,SyntaxError,SyntaxError")
      throw new Error(`Test262 Runner direct eval of a private name in a class-body arrow expected 1,8,SyntaxError,SyntaxError, got: ${evalPrivateArrowOut}`);

    const wideCapturedBlockSrc = join(tmp, "wide-captured-block.js");
    writeFileSync(
      wideCapturedBlockSrc,
      [
        "let readWideBlock;",
        "{",
        ...wideNames.map((name, index) => `  let ${name} = ${index};`),
        `  readWideBlock = () => [${wideNames.join(", ")}];`,
        "}",
        "const wideValues = readWideBlock();",
        "wideValues[0] + wideValues[259];",
        "",
      ].join("\n"),
    );
    const wideCapturedBlockOut = await $`${BARE} --print ${wideCapturedBlockSrc} 2>&1`.text();
    if (wideCapturedBlockOut.trim() !== "259")
      throw new Error(`Bare wide captured block expected 259, got: ${wideCapturedBlockOut}`);

    const thrownObjectSrc = join(tmp, "throw-test262-object.js");
    writeFileSync(
      thrownObjectSrc,
      [
        "function Test262Error(message) {",
        "  this.message = message || '';",
        "}",
        "Test262Error.prototype.toString = function () {",
        "  return 'Test262Error: ' + this.message;",
        "};",
        "throw new Test262Error('issue 830 readable failure message');",
        "",
      ].join("\n"),
    );
    const thrownObject = await $`cat ${thrownObjectSrc} | ${BARE} - --compat-function 2>&1`.nothrow();
    const thrownObjectOut = thrownObject.text();
    if (thrownObject.exitCode === 0)
      throw new Error("Bare thrown Test262Error-style object should fail");
    if (!thrownObjectOut.includes("issue 830 readable failure message"))
      throw new Error(`Bare thrown Test262Error-style object should report its message, got: ${thrownObjectOut}`);
    if (thrownObjectOut.trim() === "[object Object]")
      throw new Error("Bare thrown Test262Error-style object should not collapse to [object Object]");

    // --compat-all regression guard: the flag was removed and must now be
    // rejected as an unknown option.
    const bareCompatAll = await $`echo 'x;' | ${BARE} --compat-all - 2>&1`.nothrow();
    const compatAllOut = bareCompatAll.stdout.toString();
    if (bareCompatAll.exitCode === 0 || !compatAllOut.includes("--compat-all"))
      throw new Error(`Bare must reject --compat-all, got exit ${bareCompatAll.exitCode}: ${compatAllOut}`);

    const bareWarningSrc = join(tmp, "warning-unsupported.js");
    writeFileSync(bareWarningSrc, "while (false) {}\n23;\n");
    const bareWarningDefault = await $`${BARE} --print ${bareWarningSrc} 2>&1`.nothrow();
    if (bareWarningDefault.exitCode === 0)
      throw new Error("Bare without warning flag should reject unsupported while");
    if (!bareWarningDefault.text().includes("SyntaxError") ||
        !bareWarningDefault.text().includes("'while' loops are not supported by default"))
      throw new Error(`Bare without warning flag should report while SyntaxError, got: ${bareWarningDefault.text()}`);

    const bareWarningFile = await $`${BARE} --print ${bareWarningSrc} --warning-unsupported-features 2>&1`.text();
    if (!bareWarningFile.includes("Warning: 'while' loops are not supported by default") ||
        !bareWarningFile.includes("23"))
      throw new Error(`Bare warning mode file path should warn and print 23, got: ${bareWarningFile}`);

    const bareWarningStdin = await $`cat ${bareWarningSrc} | ${BARE} --print - --warning-unsupported-features 2>&1`.text();
    if (!bareWarningStdin.includes("Warning: 'while' loops are not supported by default") ||
        !bareWarningStdin.includes("23"))
      throw new Error(`Bare warning mode stdin should warn and print 23, got: ${bareWarningStdin}`);

    const shadowWarningSrc = join(tmp, "shadow-warning.js");
    writeFileSync(
      shadowWarningSrc,
      [
        "const realm = new ShadowRealm();",
        "const fromEvaluate = realm.evaluate('while (false) {} 23;');",
        "const fromEval = realm.evaluate(\"eval('while (false) {} 24;')\");",
        "print(fromEvaluate + fromEval);",
        "",
      ].join("\n"),
    );
    const shadowWarningProc = Bun.spawnSync(
      [TEST262RUNNER, "--eval-host", "--warning-unsupported-features"],
      {
        stdin: readFileSync(shadowWarningSrc),
        stdout: "pipe",
        stderr: "pipe",
      },
    );
    const shadowWarningOut =
      shadowWarningProc.stdout.toString() + shadowWarningProc.stderr.toString();
    if (shadowWarningProc.exitCode !== 0 ||
        !containsLine(shadowWarningOut, "47"))
      throw new Error(`ShadowRealm child realm should inherit warning-unsupported-features, got: ${shadowWarningOut}`);

    const forSrc = join(tmp, "use-for.js");
    writeFileSync(forSrc, "let s = 0;\nfor (let i = 1; i <= 5; i++) { s = s + i; }\ns;\n");
    const forOut = await $`${BARE} --print ${forSrc} --compat-traditional-for-loop 2>&1`.text();
    if (forOut.trim() !== "15") throw new Error(`Bare --compat-traditional-for-loop expected 15, got: ${forOut}`);

    const forInSrc = join(tmp, "use-for-in.js");
    writeFileSync(forInSrc, "const obj = { a: 1, b: 2 };\nlet out = '';\nfor (const k in obj) { out = out + k; }\nout;\n");
    const forInOut = await $`${BARE} --print ${forInSrc} --compat-for-in-loop 2>&1`.text();
    if (forInOut.trim() !== "ab") throw new Error(`Bare --compat-for-in-loop expected ab, got: ${forInOut}`);

    const annexBSrc = join(tmp, "use-annex-b.js");
    writeFileSync(annexBSrc, "typeof escape === 'function' ? escape('a b') + ' ' + unescape('%41%u0042') : typeof escape;\n");
    const annexBOffOut = await $`${BARE} --print ${annexBSrc} 2>&1`.text();
    if (annexBOffOut.trim() !== "undefined") throw new Error(`Bare without --compat-annex-b expected undefined, got: ${annexBOffOut}`);
    const annexBOut = await $`${BARE} --print ${annexBSrc} --compat-annex-b 2>&1`.text();
    if (annexBOut.trim() !== "a%20b AB") throw new Error(`Bare --compat-annex-b expected a%20b AB, got: ${annexBOut}`);
    const annexBLoaderOut = await $`${RUNNER} --print ${annexBSrc} --compat-annex-b 2>&1`.text();
    if (!containsLine(annexBLoaderOut, "a%20b AB")) throw new Error(`Loader --compat-annex-b expected a%20b AB, got: ${annexBLoaderOut}`);

    const whileSrc = join(tmp, "use-while.js");
    writeFileSync(whileSrc, "let s = 0;\nlet i = 1;\nwhile (i <= 5) { s = s + i; i++; }\ns;\n");
    const whileOut = await $`${BARE} --print ${whileSrc} --compat-while-loops 2>&1`.text();
    if (whileOut.trim() !== "15") throw new Error(`Bare --compat-while-loops expected 15, got: ${whileOut}`);

    const whileLoaderOut = await $`${RUNNER} --print ${whileSrc} --compat-while-loops 2>&1`.text();
    if (!containsLine(whileLoaderOut, "15")) throw new Error(`Loader --compat-while-loops expected 15, got: ${whileLoaderOut}`);

    const looseSrc = join(tmp, "use-loose.js");
    writeFileSync(looseSrc, '"1" == 1;\n');
    const loaderLoose = await $`${RUNNER} --print ${looseSrc} --compat-loose-equality 2>&1`.text();
    if (!containsLine(loaderLoose, "true")) throw new Error(`Loader --compat-loose-equality expected true, got: ${loaderLoose}`);

    const looseTest = join(tmp, "use-loose-test.js");
    writeFileSync(looseTest, 'test("loose equality", () => { expect("1" == 1).toBe(true); });\n');
    const trLoose = await $`${TESTRUNNER} ${looseTest} --compat-loose-equality --no-progress 2>&1`.text();
    if (!trLoose.includes("Passed: 1")) throw new Error(`TestRunner --compat-loose-equality expected Passed: 1, got: ${trLoose}`);

    const looseOut = join(tmp, "loose.gbc");
    await $`${BUNDLER} ${looseSrc} --compat-loose-equality --output=${looseOut}`.quiet();
    if (!existsSync(looseOut)) throw new Error("Bundler --compat-loose-equality should compile");

    const bareLoose = await $`${BARE} --print ${looseSrc} --compat-loose-equality 2>&1`.text();
    if (bareLoose.trim() !== "true") throw new Error(`Bare --compat-loose-equality expected true, got: ${bareLoose}`);
  } finally {
    clean(tmp);
  }
}

// -- Block-level function var binding skipped on a collision -------------------
//
// ES2026 §10.2.11 FunctionDeclarationInstantiation and §16.1.7
// GlobalDeclarationInstantiation, web-compat steps (Annex B.3.2.1 and B.3.2.2
// up to ES2025): a non-strict block-level function declaration gets a var
// binding only if no parameter has its name and `var` in its place would not
// be an early error; B.3.3 puts an `if` clause's function declaration in a
// block of its own.

console.log("Block-level function var binding skipped on a collision...");
{
  const cases: [string, string, string][] = [
    ["parameter with a default", "(() => { function g(a, b = 2) { { function a() {} } return typeof a + ':' + a; } return g(1); })()", "number:1"],
    ["parameter with a default, switch case", "(() => { function g(a, b = 2) { switch (1) { case 1: function a() {} } return a; } return g(1); })()", "1"],
    ["parameter, if clause", "(() => { function h(a) { if (true) function a() {} return a; } return h(1); })()", "1"],
    ["parameter, else clause", "(() => { function h(a) { if (false) ; else function a() {} return a; } return h(1); })()", "1"],
    ["if clause without a collision", "(() => { function h() { const before = typeof q; if (true) function q() { return 7; } return before + ':' + q(); } return h(); })()", "undefined:7"],
    ["let in an enclosing block", "(() => { function f() { { let a = 1; { function a() {} } } return typeof a; } return f(); })()", "undefined"],
    ["let in an enclosing block, parameter with a default", "(() => { function f(x = 0) { { let a = 1; { function a() {} } } try { return a; } catch (e) { return e.name; } } return f(); })()", "ReferenceError"],
    ["let loop head", "(() => { function f() { for (let a of [1]) { { function a() {} } } return typeof a; } return f(); })()", "undefined"],
    ["destructured catch parameter", "(() => { function f() { try { throw {}; } catch ({ a }) { { function a() {} } } return typeof a; } return f(); })()", "undefined"],
    ["two declarations in one block both keep it", "(() => { function f() { { function a() { return 1; } function a() { return 2; } } return a(); } return f(); })()", "2"],
    ["same name in a nested block", "(() => { function f() { { function a() { return 1; } { function a() { return 2; } } } return a(); } return f(); })()", "1"],
  ];
  const source = cases
    .map(([, expression]) => `console.log(String(${expression}));`)
    .join("\n") + "\n";
  const flags = ["--compat-var", "--compat-function", "--compat-non-strict-mode"];
  const { exitCode, json } = runLoaderJson(source, flags);
  if (exitCode !== 0)
    throw new Error(`Block function cases should run, got: ${JSON.stringify(json.error)}`);
  cases.forEach(([label, , expected], index) => {
    if (json.output[index] !== expected)
      throw new Error(`Block function (${label}) expected ${expected}, got: ${json.output[index]}`);
  });

  const scriptRun = runLoaderJson(
    "const k = 1;\nif (true) function k() {}\nconsole.log(typeof k + ':' + k);\n",
    flags,
  );
  if (scriptRun.exitCode !== 0 || scriptRun.json.output[0] !== "number:1")
    throw new Error(`Script if-clause function named like a const expected number:1, got: ${JSON.stringify(scriptRun.json)}`);
}

// -- --compat-non-strict-mode (Loader + Bundler + TestRunner + Bare) -----------

console.log("--compat-non-strict-mode (Loader + Bundler + TestRunner + Bare)...");
{
  const tmp = mkdtemp("goccia-nonstrict-");
  try {
    const src = join(tmp, "use-nonstrict.js");
    writeFileSync(
      src,
      [
        "function f(a) {",
        "  if (this !== globalThis) return -1;",
        "  with ({ extra: 5 }) {",
        "    return arguments.length + extra;",
        "  }",
        "}",
        "f(1, 2);",
      ].join("\n") + "\n",
    );

    const loaderOut = await $`${RUNNER} --print ${src} --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (!containsLine(loaderOut, "7")) throw new Error(`Loader --compat-non-strict-mode expected 7, got: ${loaderOut}`);

    const outPath = join(tmp, "use-nonstrict.gbc");
    await $`${BUNDLER} ${src} --output=${outPath} --compat-function --compat-non-strict-mode --compat-arguments-object`.quiet();
    if (!existsSync(outPath)) throw new Error("Bundler --compat-non-strict-mode should compile");

    const deleteSrc = join(tmp, "delete-nonstrict.js");
    writeFileSync(
      deleteSrc,
      [
        "let binding = 1;",
        "globalThis.tempDeleteName = 1;",
        "const obj = {};",
        'Object.defineProperty(obj, "fixed", { value: 1, configurable: false });',
        "(delete binding ? 1 : 0) + (delete obj.fixed ? 2 : 0) + (delete missingName ? 4 : 0) + (delete tempDeleteName ? 8 : 0);",
      ].join("\n") + "\n",
    );
    const deleteOutPath = join(tmp, "delete-nonstrict.gbc");
    await $`${BUNDLER} ${deleteSrc} --output=${deleteOutPath} --compat-non-strict-mode`.quiet();
    const bundledDeleteOut = await $`${RUNNER} --print ${deleteOutPath} --compat-non-strict-mode 2>&1`.text();
    if (!containsLine(bundledDeleteOut, "12")) throw new Error(`Bundled non-strict delete expected 12, got: ${bundledDeleteOut}`);

    const assignmentSrc = join(tmp, "assignment-nonstrict.js");
    writeFileSync(
      assignmentSrc,
      [
        "const obj = {};",
        'Object.defineProperty(obj, "fixed", { value: 1, writable: false });',
        "obj.fixed = 2;",
        "const withObj = {};",
        'Object.defineProperty(withObj, "value", { value: 1, writable: false });',
        "with (withObj) {",
        "  value = 2;",
        "}",
        "looseCreated = 3;",
        "let calledThis;",
        "const receiverObj = { x: 4, tag(strings) { return this.x; } };",
        "with (receiverObj) {",
        "  calledThis = tag``;",
        "}",
        "obj.fixed + withObj.value + looseCreated + calledThis;",
      ].join("\n") + "\n",
    );
    const assignmentOutPath = join(tmp, "assignment-nonstrict.gbc");
    await $`${BUNDLER} ${assignmentSrc} --output=${assignmentOutPath} --compat-non-strict-mode`.quiet();
    const bundledAssignmentOut = await $`${RUNNER} --print ${assignmentOutPath} --compat-non-strict-mode 2>&1`.text();
    if (!containsLine(bundledAssignmentOut, "9")) throw new Error(`Bundled non-strict assignment expected 9, got: ${bundledAssignmentOut}`);

    const moduleWithSrc = join(tmp, "module-with.js");
    writeFileSync(moduleWithSrc, "with ({ x: 1 }) { x; }\n");
    const moduleWith = await $`${RUNNER} ${moduleWithSrc} --source-type=module --compat-non-strict-mode 2>&1`.nothrow();
    const moduleWithOutput = moduleWith.text();
    if (moduleWith.exitCode === 0 || !moduleWithOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Module with should fail as strict code, got: ${moduleWithOutput}`);
    const moduleWithWarning = await $`${RUNNER} ${moduleWithSrc} --source-type=module --compat-non-strict-mode --warning-unsupported-features 2>&1`.nothrow();
    const moduleWithWarningOutput = moduleWithWarning.text();
    if (moduleWithWarning.exitCode === 0 || !moduleWithWarningOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Module with should remain strict even in warning mode, got: ${moduleWithWarningOutput}`);

    const testSrc = join(tmp, "test-nonstrict.js");
    writeFileSync(
      testSrc,
      [
        "function f(a) {",
        "  if (this !== globalThis) return -1;",
        "  with ({ extra: 5 }) {",
        "    return arguments.length + extra;",
        "  }",
        "}",
        'test("non-strict mode compat", () => { expect(f(1, 2)).toBe(7); });',
      ].join("\n") + "\n",
    );
    const trOut = await $`${TESTRUNNER} ${testSrc} --no-progress --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (!trOut.includes("Passed: 1")) throw new Error(`TestRunner --compat-non-strict-mode expected Passed: 1, got: ${trOut}`);

    const bareOut = await $`${BARE} --print ${src} --compat-function --compat-non-strict-mode --compat-arguments-object 2>&1`.text();
    if (bareOut.trim() !== "7") throw new Error(`Bare --compat-non-strict-mode expected 7, got: ${bareOut}`);

    const staticSetterSrc = join(tmp, "static-setter-nonstrict.js");
    writeFileSync(
      staticSetterSrc,
      [
        "let captured = 0;",
        'const computed = "computed";',
        'const symbolKey = Symbol("staticSetter");',
        "class C {",
        "  static set eval(value) { captured = captured + value; }",
        "  static set arguments(value) { captured = captured + value * 10; }",
        "  static set [computed](value) { captured = captured + value * 100; }",
        "  static set [symbolKey](value) { captured = captured + value * 1000; }",
        "}",
        "C.eval = 1;",
        "C.arguments = 2;",
        "C[computed] = 3;",
        "C[symbolKey] = 4;",
        "captured;",
      ].join("\n") + "\n",
    );
    const staticSetterBareOut = await $`${BARE} --print ${staticSetterSrc} --compat-non-strict-mode 2>&1`.text();
    if (staticSetterBareOut.trim() !== "4321")
      throw new Error(`Bare static setters in non-strict mode expected 4321, got: ${staticSetterBareOut}`);

    const noFlagOut = join(tmp, "no-flag.gbc");
    const bundleNoFlag = await $`${BUNDLER} ${src} --compat-function --output=${noFlagOut} 2>&1`.nothrow();
    if (bundleNoFlag.exitCode === 0)
      throw new Error(`Bundler without --compat-non-strict-mode should fail unsupported with by default`);
    if (!bundleNoFlag.text().includes("SyntaxError") ||
        !bundleNoFlag.text().includes("'with' statements require --compat-non-strict-mode"))
      throw new Error(`Bundler without --compat-non-strict-mode should report SyntaxError, got: ${bundleNoFlag.text()}`);
    if (existsSync(noFlagOut)) throw new Error("Bundler default failure should not write bytecode output");

    const warningOut = join(tmp, "warning-with.gbc");
    await $`${BUNDLER} ${src} --compat-function --warning-unsupported-features --output=${warningOut}`.quiet();
    if (!existsSync(warningOut)) throw new Error("Bundler --warning-unsupported-features should preserve warning recovery mode");
  } finally {
    clean(tmp);
  }
}

// -- Body var named like a parameter starts with its value ---------------------
//
// ES2026 §10.2.11 FunctionDeclarationInstantiation step 30.e.i.4: with an
// expression in the parameter list, a body var named like a parameter (or
// `arguments`) is a binding of its own that starts with that binding's value.
// These cases read the var before the body assigns it.

console.log("Body var named like a parameter starts with its value...");
{
  const cases: [string, string, string][] = [
    ["var without an initializer", "((a, f = () => a) => { var a; return [a, f()]; })(1)", "1,1"],
    ["default closure writes the parameter", "((a, f = () => { a = 7; }) => { var a; f(); return a; })(1)", "1"],
    ["defaulted parameter", "((a = 2, f = () => a) => { var a; return [a, f()]; })()", "2,2"],
    ["destructured parameter with a default", "(({ a = 0 }) => { var a; return a; })({ a: 1 })", "1"],
    ["function declaration", "(() => { function h(a, f = () => a) { var a; return a; } return h(1); })()", "1"],
    ["method", "({ m(a, f = () => a) { var a; return [a, f()]; } }).m(3)", "3,3"],
    ["var named arguments", "(() => { function g(a, f = () => arguments) { var arguments; return [arguments === f(), arguments.length]; } return g(1, undefined, 3); })()", "true,3"],
    ["unmapped arguments object", "(() => { function g(a, b = 1) { arguments[0] = 9; var a; return [a, arguments[0]]; } return g(1); })()", "1,9"],
    ["generator copies when called", "(() => { let w; function* g(a, f = (w = () => { a = 9; })) { var a; yield a; } const it = g(1); w(); return it.next().value; })()", "1"],
  ];
  const source = cases
    .map(([, expression]) => `console.log(String(${expression}));`)
    .join("\n") + "\n";
  const { exitCode, json } = runLoaderJson(source, [
    "--compat-var",
    "--compat-function",
    "--compat-arguments-object",
    "--compat-non-strict-mode",
  ]);
  if (exitCode !== 0)
    throw new Error(`Body var cases should run, got: ${JSON.stringify(json.error)}`);
  cases.forEach(([label, , expected], index) => {
    if (json.output[index] !== expected)
      throw new Error(`Body var (${label}) expected ${expected}, got: ${json.output[index]}`);
  });

  // A direct eval in the body reaches the body var; one in a default reaches
  // the parameter.
  const evalSource = [
    'function read(a, f = () => a) { var a = 5; return [eval("a"), f()]; }',
    'function write(a, f = () => a) { var a = 5; eval("a = 6"); return [a, f()]; }',
    'function inDefault(a, f = eval("() => a")) { var a = 5; return [a, f()]; }',
    'function closureWrites(a, f = () => { a = 7; }) { var a = 3; f(); return [eval("a"), a]; }',
    'function unassigned(a, f = () => a) { var a; return [eval("a"), f()]; }',
    'const shared = [read(1), write(1), inDefault(1), closureWrites(1)].map((r) => r.join(":")).join(",");',
    'print(shared);',
    'print(unassigned(1).join(":"));',
    "",
  ].join("\n");
  const evalRun = Bun.spawnSync([TEST262RUNNER, "--eval-host"], {
    stdin: new TextEncoder().encode(evalSource),
    stdout: "pipe",
    stderr: "pipe",
  });
  const evalOut = normalizeLineEndings(evalRun.stdout.toString()).split("\n");
  const evalAll = evalRun.stdout.toString() + evalRun.stderr.toString();
  if (evalRun.exitCode !== 0 || evalOut[0] !== "5:1,6:1,5:1,3:3")
    throw new Error(`Test262 Runner direct eval with a body var expected 5:1,6:1,5:1,3:3, got: ${evalAll}`);
  if (evalOut[1] !== "1:1")
    throw new Error(`Test262 Runner direct eval of an unassigned body var expected 1:1, got: ${evalAll}`);
}

// -- --source-type and .mjs module inference -----------------------------------

console.log("--source-type and .mjs module inference (Loader + TestRunner + Bundler)...");
{
  const tmp = mkdtemp("goccia-source-type-");
  try {
    const moduleEntry = join(tmp, "entry.mjs");
    writeFileSync(moduleEntry, "this === undefined;\n");

    const loaderMjs = await $`${RUNNER} --print ${moduleEntry} 2>&1`.text();
    if (!containsLine(loaderMjs, "true")) throw new Error(`Loader .mjs should infer module source, got: ${loaderMjs}`);

    const loaderScriptOverride = await $`${RUNNER} --print ${moduleEntry} --source-type=script 2>&1`.text();
    if (!containsLine(loaderScriptOverride, "false"))
      throw new Error(`Loader --source-type=script should override .mjs inference, got: ${loaderScriptOverride}`);

    const testModuleEntry = join(tmp, "entry-test.mjs");
    writeFileSync(
      testModuleEntry,
      [
        "const topLevelThis = this;",
        "const metaUrl = import.meta.url;",
        'test(".mjs top-level this", () => { expect(topLevelThis).toBeUndefined(); });',
        'test(".mjs import.meta", () => { expect(metaUrl.endsWith("entry-test.mjs")).toBe(true); });',
      ].join("\n") + "\n",
    );
    const testRunnerMjs = await $`${TESTRUNNER} ${testModuleEntry} --no-progress 2>&1`.text();
    if (!testRunnerMjs.includes("Passed: 2")) throw new Error(`TestRunner .mjs expected Passed: 2, got: ${testRunnerMjs}`);

    const testScriptOverride = join(tmp, "entry-test-script.mjs");
    writeFileSync(
      testScriptOverride,
      [
        "const topLevelThis = this;",
        'test(".mjs script override", () => { expect(topLevelThis === undefined).toBe(false); });',
      ].join("\n") + "\n",
    );
    const testRunnerScript = await $`${TESTRUNNER} ${testScriptOverride} --source-type=script --no-progress 2>&1`.text();
    if (!testRunnerScript.includes("Passed: 1"))
      throw new Error(`TestRunner --source-type=script expected Passed: 1, got: ${testRunnerScript}`);

    const strictModule = join(tmp, "strict-module.mjs");
    writeFileSync(strictModule, "with ({ x: 1 }) { x; }\n");
    const bundleModule = await $`${BUNDLER} ${strictModule} --compat-non-strict-mode 2>&1`.nothrow();
    const bundleModuleOutput = bundleModule.text();
    if (bundleModule.exitCode === 0 || !bundleModuleOutput.includes("'with' statements are not allowed in strict mode"))
      throw new Error(`Bundler .mjs should infer strict module source, got: ${bundleModuleOutput}`);

    const bundledScriptOut = join(tmp, "strict-module-script.gbc");
    await $`${BUNDLER} ${strictModule} --source-type=script --compat-non-strict-mode --output=${bundledScriptOut}`.quiet();
    if (!existsSync(bundledScriptOut)) throw new Error("Bundler --source-type=script should override .mjs inference and write bytecode");
  } finally {
    clean(tmp);
  }
}

// -- Entry source-type detection (#1626, ADR 0140) ---------------------------

// With no explicit source type, an entry that has a top-level import or
// export declaration or valid top-level await runs as a Module. Node.js
// prints "y" for cyc.js and "yh" for fcyc.js.
console.log("Entry source-type detection (Runner, TestRunner, BenchmarkRunner, Bundler, LoaderBare, sandbox)...");
{
  const tmp = mkdtemp("goccia-detect-module-");
  try {
    const files: Record<string, string> = {
      "cyc.js": 'import { y } from "./d.js";\nexport const h = () => "h";\nconsole.log(y);\n',
      "d.js": 'import { h } from "./cyc.js";\nexport const y = "y";\nexport const z = () => h();\n',
      "fcyc.js": 'import { y2 } from "./fd.js";\nexport function h2() { return "h"; }\nconsole.log(y2);\n',
      "fd.js": 'import { h2 } from "./fcyc.js";\nexport const y2 = "y" + h2();\n',
      "script.js": 'var scriptVar = 1;\nconsole.log(typeof globalThis.scriptVar, this === globalThis);\n',
      "dynamic.js":
        'var dynamicVar = 1;\nimport("./lib.js").then((m) => console.log(m.v, typeof globalThis.dynamicVar, this === globalThis));\n',
      "await-dynamic.js":
        'var dynamicVar = 1;\nconst m = await import("./lib.js");\nconsole.log(m.v, typeof globalThis.dynamicVar, this === globalThis);\n',
      "lib.js": 'export const v = "lib";\n',
      // A detected Module's top-level lexical declarations stay in the module
      // scope, so one may share a global's name. CompileModule clears
      // GlobalBackedTopLevel for it, which the host set for a Script entry.
      "shadow-global.js": "export {};\nconst NaN = 1;\nconsole.log(NaN);\n",
      "meta.js": "console.log(typeof import.meta.url);\n",
      "with-module.js": "with ({}) {}\nexport {};\n",
      "with-script.js": "with ({}) {}\n",
      "cycle.test.js":
        'import { readEntry } from "./cycle-peer.js";\nexport const entry = "entry";\ntest("peer reads the entry", () => { expect(readEntry()).toBe("entry"); });\n',
      "cycle-peer.js": 'import { entry } from "./cycle.test.js";\nexport const readEntry = () => entry;\n',
      "bench-cycle.js":
        'import { bench } from "goccia:microbench";\nimport { y } from "./bench-peer.js";\nexport const h = () => "h";\nconst read = y;\nbench("reads the peer", () => read + h());\n',
      "bench-peer.js": 'import { h } from "./bench-cycle.js";\nexport const y = "y";\n',
    };
    for (const [name, source] of Object.entries(files)) writeFileSync(join(tmp, name), source);

    const run = (binary: string, args: string[]) => {
      const proc = Bun.spawnSync([resolve(binary), ...args], { cwd: tmp, stdout: "pipe", stderr: "pipe" });
      return {
        exitCode: proc.exitCode,
        stdout: normalizeLineEndings(proc.stdout.toString()),
        output: normalizeLineEndings(proc.stdout.toString() + proc.stderr.toString()),
      };
    };
    const expectLine = (label: string, result: ReturnType<typeof run>, line: string) => {
      if (result.exitCode !== 0 || !containsLine(result.stdout, line))
        throw new Error(`${label}: expected exit 0 and the line "${line}", got exit ${result.exitCode}: ${result.output}`);
    };
    const expectFailure = (label: string, result: ReturnType<typeof run>, text: string) => {
      if (result.exitCode === 0 || !result.output.includes(text))
        throw new Error(`${label}: expected a failure mentioning ${JSON.stringify(text)}, got exit ${result.exitCode}: ${result.output}`);
    };

    expectLine("detected module entry in a cycle", run(RUNNER, ["cyc.js"]), "y");
    expectLine("detected module entry's function in a cycle",
      run(RUNNER, ["fcyc.js", "--compat-function"]), "yh");
    expectLine("detected module may bind a global's name at top level",
      run(RUNNER, ["shadow-global.js"]), "1");
    expectLine("entry without module syntax stays a Script",
      run(RUNNER, ["script.js", "--compat-var"]), "number true");
    expectLine("entry with only dynamic import() stays a Script",
      run(RUNNER, ["dynamic.js", "--compat-var"]), "lib number true");
    expectLine("top-level await import() selects a Module",
      run(RUNNER, ["await-dynamic.js", "--compat-var"]), "lib undefined false");
    expectFailure("import.meta alone stays a Script error",
      run(RUNNER, ["meta.js"]), "import.meta is only allowed in module code");
    // An explicit Script keeps today's behaviour: d.js's import of the entry
    // loads a second copy of it, which reads y in its temporal dead zone.
    expectFailure("explicit --source-type=script keeps the Script entry",
      run(RUNNER, ["cyc.js", "--source-type=script"]), "Cannot access 'y' before initialization");
    expectFailure("a detected Module is strict under --compat-non-strict-mode",
      run(RUNNER, ["with-module.js", "--compat-non-strict-mode"]),
      "'with' statements are not allowed in strict mode");

    const testRun = run(TESTRUNNER, ["cycle.test.js", "--no-progress"]);
    if (testRun.exitCode !== 0 || !testRun.output.includes("Passed: 1"))
      throw new Error(`TestRunner detected module test file in a cycle: ${testRun.output}`);

    const benchRun = run(BENCHRUNNER, ["bench-cycle.js", "--no-progress"]);
    if (benchRun.exitCode !== 0 || !benchRun.output.includes("reads the peer"))
      throw new Error(`BenchmarkRunner detected module entry in a cycle: ${benchRun.output}`);

    const sandboxRun = run(RUNNER, ["--copy", `${tmp}=/`, "--entry=/cyc.js"]);
    expectLine("sandbox detected module entry in a cycle", sandboxRun, "y");

    // Stdin has no file name, so its goal is detected too, as Node.js does.
    const stdinRun = Bun.spawnSync([resolve(RUNNER), "--print", "-"], {
      cwd: tmp,
      stdin: new TextEncoder().encode("export {};\nthis === undefined;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    if (stdinRun.exitCode !== 0 || !containsLine(stdinRun.stdout.toString(), "true"))
      throw new Error(`stdin with an export should run as a Module: ${stdinRun.stdout}${stdinRun.stderr}`);

    // A REPL line is not an entry file: it stays a Script, so the lines keep
    // sharing one global environment.
    const repl = Bun.spawnSync([resolve(REPL)], {
      cwd: tmp,
      stdin: new TextEncoder().encode("export const replValue = 1; this === globalThis;\nreplValue + 1;\n"),
      stdout: "pipe",
      stderr: "pipe",
    });
    const replLines = normalizeLineEndings(repl.stdout.toString())
      .split("\n")
      .map((line) => line.replace(/^(> )+/, ""));
    if (repl.exitCode !== 0 || !replLines.includes("true") || !replLines.includes("2"))
      throw new Error(`REPL should run module syntax as a Script: ${repl.stdout}${repl.stderr}`);

    for (const [source, expected] of [
      ["export {};\nthis === undefined;\n", "true"],
      ["this === undefined;\n", "false"],
    ]) {
      const file = join(tmp, `bare-${expected}.js`);
      writeFileSync(file, source);
      const bare = run(BARE, ["--print", file]);
      if (bare.exitCode !== 0 || bare.stdout.trim() !== expected)
        throw new Error(`LoaderBare detection for ${JSON.stringify(source)} expected ${expected}, got: ${bare.output}`);
    }

    expectFailure("Bundler: a detected Module is strict",
      run(BUNDLER, ["with-module.js", "--compat-non-strict-mode", "--output=with-module.gbc"]),
      "'with' statements are not allowed in strict mode");
    const bundledScript = run(BUNDLER, ["with-script.js", "--compat-non-strict-mode", "--output=with-script.gbc"]);
    if (bundledScript.exitCode !== 0 || !existsSync(join(tmp, "with-script.gbc")))
      throw new Error(`Bundler should bundle a detected Script with a with statement: ${bundledScript.output}`);
  } finally {
    clean(tmp);
  }
}

// -- --timeout (Loader: infinite loop) ------------------------------------------

console.log("--timeout...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--timeout=50"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

// -- --timeout with exactly one place left to notice it ---------------------------
//
// The deadline is polled from several places: backward jumps and calls in the
// bytecode loop, the entry of a native-to-bytecode call, and every value
// allocation. Each fixture below leaves one of them as the only poll that
// runs, so that removing it stops the script from timing out.

{
  // Function-local int32 arithmetic stays in registers, so the loops allocate
  // nothing and call nothing. (At the top level the counter would be a global
  // binding, and each new value an allocation.) The recursive calls return
  // numbers that fit a register and allocate nothing either.
  const onePoll: Array<[string, string, string[]]> = [
    ["backward jump", "const spin = () => { let i = 0; while (true) { i = (i + 1) | 0; } }; spin();\n", ["--compat-while-loops"]],
    ["backward conditional jump", "const spin = () => { let i = 0; do { i = (i + 1) | 0; } while (true); }; spin();\n", ["--compat-while-loops"]],
    ["call", "const fib = (n) => n < 2 ? n : fib(n - 1) + fib(n - 2); fib(45);\n", []],
    ["method call", "const o = { fib(n) { return n < 2 ? n : o.fib(n - 1) + o.fib(n - 2); } }; o.fib(45);\n", []],
    // A native consumer drives a bytecode `next` that returns the same result
    // object every time: nothing is allocated, no jump goes backward and no
    // bytecode call is made, so only the poll at the native-to-bytecode entry
    // is left. Set keeps one element, so memory does not grow.
    ["native-to-bytecode entry", "const res = { done: false, value: 0 }; const it = { [Symbol.iterator]() { return this; }, next: () => res }; new Set(it);\n", []],
    // The same through a bound function with nine bound arguments, which
    // enters through the heap-argument path.
    ["native-to-bytecode entry with heap arguments", "const res = { done: false, value: 0 }; const f = (a, b, c, d, e, g, h, i, j) => res; const it = { [Symbol.iterator]() { return this; }, next: f.bind(null, 1, 2, 3, 4, 5, 6, 7, 8, 9) }; new Set(it);\n", []],
    // split() builds four million strings inside one native call that has no
    // poll of its own; the poll in the value allocation is what ends it.
    // Without that poll the call completes, in well over a second.
    ["value allocation", 'const parts = "ab,".repeat(4000000).split(","); parts.length;\n', []],
  ];
  for (const [what, source, flags] of onePoll) {
    console.log(`--timeout (only the ${what} poll is left)...`);
    const { exitCode, json } = runLoaderJson(source, ["--timeout=50", ...flags], { timeout: 10_000 });
    if (exitCode !== 1) throw new Error(`Timeout through the ${what} poll: exit code should be 1, got ${exitCode}`);
    if (json.error?.type !== "TimeoutError") throw new Error(`Timeout through the ${what} poll: expected TimeoutError, got ${json.error?.type}`);
  }
}

// -- --timeout inside long-running NATIVE operations ----------------------------
//
// A single JS statement can stall inside one native call (dense hole-fill for
// a huge array index, or a backtracking regex scan over a long subject).  The
// deadline must interrupt those loops too, not just the dispatch loop between
// JS-level steps.  Keep the regex fixture away from raw negative-scan
// prefilters: those are supposed to complete quickly.

const regexTimeoutScan = 'const s = "a".repeat(2000000); /a*b/.test(s);\n';

console.log("--timeout (native sparse-array fill)...");
{
  // Far single-index writes route to sparse storage and complete instantly;
  // the Array constructor still materializes dense holes, so it stalls.
  // --max-memory=0 lifts the budget: 2**30 pointers is exactly the 8 GiB
  // default cap, so the allocation gate would otherwise refuse the request
  // up front and this would assert the memory limit instead of the deadline.
  // The stall is still bounded — hole extension polls the deadline as it
  // grows, so only the fraction allocated within 50ms is ever committed.
  const fill = "const x = new Array(2 ** 30); x.length;\n";
  const { exitCode, json } = runLoaderJson(fill, ["--timeout=50", "--max-memory=0"], { timeout: 10_000 });
  // An engine that keeps the holes sparse makes the array at once: nothing to stall.
  const completed = exitCode === 0 && json.files?.[0]?.result === 2 ** 30;
  if (!completed && exitCode !== 1) throw new Error(`Array-fill timeout exit code should be 1, got ${exitCode}`);
  if (!completed && json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

console.log("--timeout and --max-instructions (Array.prototype.fill over a length above 2**31 - 1)...");
if (!canCapMemory) {
  console.log("  skipped: no process memory cap on this platform");
} else {
  // A length above 2**31 - 1 takes fill's sparse path, which assigns every
  // index as a property. That loop is bound only by the JS-controlled length,
  // so the deadline and the instruction budget have to reach it
  // (the chokepoint rule in Goccia.Timeout).
  const cases: [string, string][] = [
    ["an Array", "const a = new Array(2 ** 31 + 10);\na.fill(1);\n0;\n"],
    ["an array-like", "Array.prototype.fill.call({ length: 2 ** 40 }, 1);\n0;\n"],
  ];
  for (const [label, source] of cases) {
    const timed = runLoaderJson(source, ["--timeout=100", "--max-memory=0"], { timeout: 10_000, memoryCapMiB: 1024 });
    if (timed.exitCode !== 1) throw new Error(`fill over ${label}: --timeout exit code should be 1, got ${timed.exitCode}`);
    if (timed.json.error?.type !== "TimeoutError") throw new Error(`fill over ${label}: expected TimeoutError, got ${timed.json.error?.type}`);
    const counted = runLoaderJson(source, ["--max-instructions=1000", "--max-memory=0"], { timeout: 10_000, memoryCapMiB: 1024 });
    if (counted.exitCode !== 1) throw new Error(`fill over ${label}: --max-instructions exit code should be 1, got ${counted.exitCode}`);
    if (counted.json.error?.type !== "InstructionLimitError") throw new Error(`fill over ${label}: expected InstructionLimitError, got ${counted.json.error?.type}`);
  }
}

console.log("--max-instructions (native loops over a huge array)...");
if (!canCapMemory) {
  console.log("  skipped: no process memory cap on this platform");
} else {
  // These loops used to poll only the deadline (through the per-element reads
  // or the allocations they make), so an instruction budget could not end
  // them: the chokepoint rule in Goccia.Timeout covers every limit.
  const huge = "const a = new Array(2 ** 31 + 10);\n";
  const cases: [string, string][] = [
    ["the dense Array constructor", "const a = new Array(2 ** 28);\n0;\n"],
    ["join", `${huge}a.join();\n0;\n`],
    ["reverse", `${huge}a.reverse();\n0;\n`],
    ["sort", `${huge}a.sort();\n0;\n`],
    ["map", `${huge}a.map(() => 1);\n0;\n`],
    ["shift", `${huge}a.shift();\n0;\n`],
    ["copyWithin", `${huge}a.copyWithin(0, 1);\n0;\n`],
    ["Array.from", `${huge}Array.from(a);\n0;\n`],
    ["spread", `${huge}const b = [...a];\n0;\n`],
  ];
  for (const [label, source] of cases) {
    const { exitCode, json } = runLoaderJson(source, ["--max-instructions=1000", "--max-memory=0"], { timeout: 10_000, memoryCapMiB: 1024 });
    // An engine that keeps the holes sparse (making, shifting or moving only
    // the elements present) has no loop over them to end: finishing within the
    // budget is right too. Anything else (a memory fault, a hang) is not.
    if (exitCode === 0) continue;
    if (exitCode !== 1) throw new Error(`${label}: --max-instructions exit code should be 1, got ${exitCode}`);
    if (json.error?.type !== "InstructionLimitError") throw new Error(`${label}: expected InstructionLimitError, got ${json.error?.type}`);
  }
}

console.log("--max-instructions (native work counted once, and by what it covers)...");
{
  // Work that already counts itself is not charged a second time by the poll.
  const json = 'JSON.parse("[" + "1,".repeat(1999) + "1]");\n';
  const parsed = runLoaderJson(json, ["--max-instructions=6500"], { timeout: 10_000 });
  if (parsed.exitCode !== 0) throw new Error(`JSON.parse under --max-instructions=6500 should succeed, got exit ${parsed.exitCode}: ${JSON.stringify(parsed.json.error)}`);
  // A fill and a spread through the iterator protocol are charged for the
  // elements they walk. The setup alone stays within the budget (a short
  // hole extension is not charged), so the operation is what trips it.
  const operations: [string, string, string, number][] = [
    ["fill of an array-like", "const o = { length: 1000 };\n", "Array.prototype.fill.call(o, 1);\n0;\n", 500],
    ["fill of an array", "const a = new Array(1000);\n", "a.fill(1);\n0;\n", 500],
    ["fill of one index at a batch boundary", "const o = { length: 1000 };\n", "Array.prototype.fill.call(o, 1, 255, 256);\n0;\n", 100],
    [
      "spread through an own Symbol.iterator",
      "const a = new Array(1000);\na[Symbol.iterator] = Array.prototype[Symbol.iterator];\n",
      "const b = [...a];\n0;\n",
      500,
    ],
  ];
  for (const [label, setup, operation, limit] of operations) {
    const budget = [`--max-instructions=${limit}`];
    const alone = runLoaderJson(setup, budget, { timeout: 10_000 });
    if (alone.exitCode !== 0) throw new Error(`${label}: its setup alone should stay within --max-instructions=${limit}, got exit ${alone.exitCode}`);
    const run = runLoaderJson(setup + operation, budget, { timeout: 10_000 });
    const trips = run.exitCode === 1 && run.json.error?.type === "InstructionLimitError";
    // One index is one write: it must not be charged a whole batch.
    if (label === "fill of one index at a batch boundary") {
      if (run.exitCode !== 0) throw new Error(`${label}: one write should stay within --max-instructions=${limit}, got exit ${run.exitCode}`);
    } else if (!trips) {
      throw new Error(`${label}: should end with InstructionLimitError, got exit ${run.exitCode}`);
    }
  }
}

console.log("--max-instructions (a far accessor index on an array)...");
{
  // The holes are extended before the descriptor is published, so the raise
  // of a poll there finds nothing the array owns (it used to free a
  // descriptor the array still held and fault).
  const define = "const a = [];\nObject.defineProperty(a, 100000, { get: () => 1, configurable: true });\na.length;\n";
  // An engine that keeps the index sparse extends no holes and finishes
  // within the budget; one that extends them must end with the limit, not a fault.
  for (const [flag, type] of [["--max-instructions=1000", "InstructionLimitError"]]) {
    const { exitCode, json } = runLoaderJson(define, [flag], { timeout: 10_000 });
    if (exitCode === 0) continue;
    if (exitCode !== 1 || json.error?.type !== type) throw new Error(`defineProperty at a far index under ${flag} should end with ${type} or finish, got exit ${exitCode}: ${JSON.stringify(json.error)}`);
  }
}

console.log("--max-instructions (drop skip through next())...");
{
  const setup = "const it = Array.prototype.values.call({ length: 5000 }).drop(4999);\n";
  const drop = setup + "it.next();\n0;\n";
  const alone = runLoaderJson(setup, ["--max-instructions=1000"], { timeout: 10_000 });
  if (alone.exitCode !== 0) throw new Error(`drop setup alone should succeed, got exit ${alone.exitCode}`);
  const { exitCode, json } = runLoaderJson(drop, ["--max-instructions=1000"], { timeout: 10_000 });
  if (exitCode !== 1 || json.error?.type !== "InstructionLimitError") throw new Error(`drop skip through next() should end with InstructionLimitError, got exit ${exitCode}`);
}

console.log("--max-instructions (an iterator object over a bound native next)...");
{
  // The consumer's step is charged however the next method is reached.
  const setup = "const src = Array.prototype.values.call({ length: 1000 });\nconst it = { next: src.next.bind(src), [Symbol.iterator]() { return this; } };\n";
  const alone = runLoaderJson(setup, ["--max-instructions=500"], { timeout: 10_000 });
  if (alone.exitCode !== 0) throw new Error(`bound-next setup alone should succeed, got exit ${alone.exitCode}`);
  for (const [label, body] of [["spread", "const b = [...it];\n0;\n"], ["a destructuring pattern", "const [...r] = it;\n0;\n"]]) {
    const { exitCode, json } = runLoaderJson(setup + body, ["--max-instructions=500"], { timeout: 10_000 });
    if (exitCode !== 1 || json.error?.type !== "InstructionLimitError") throw new Error(`${label} over a bound native next should end with InstructionLimitError, got exit ${exitCode}: ${JSON.stringify(json.error)}`);
  }
}

console.log("--max-instructions (Iterator.prototype.toArray over a native iterator)...");
{
  const setup = "const it = Array.prototype.values.call({ length: 5000 });\n";
  const alone = runLoaderJson(setup, ["--max-instructions=1000"], { timeout: 10_000 });
  if (alone.exitCode !== 0) throw new Error(`toArray setup alone should succeed, got exit ${alone.exitCode}`);
  const { exitCode, json } = runLoaderJson(setup + "it.toArray();\n0;\n", ["--max-instructions=1000"], { timeout: 10_000 });
  if (exitCode !== 1 || json.error?.type !== "InstructionLimitError") throw new Error(`toArray over a native iterator should end with InstructionLimitError, got exit ${exitCode}`);
}

console.log("--timeout (native regex scan)...");
{
  const { exitCode, json } = runLoaderJson(regexTimeoutScan, ["--timeout=50"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Regex-scan timeout exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
}

// -- --timeout must escape promise boundaries (dynamic import) ------------------
//
// The deadline abort must not be convertible into a JS-catchable rejection:
// a module that stalls inside import() previously surfaced as a caught
// rejection and the script "succeeded" with exit 0, defeating --timeout.

{
  console.log("--timeout (dynamic import stall)...");
  const tmp = mkdtemp("goccia-timeout-import-");
  try {
    const dep = join(tmp, "dep.js");
    writeFileSync(dep, `${regexTimeoutScan}export const ready = true;\n`);
    const main = join(tmp, "main.js");
    writeFileSync(main, 'import("./dep.js").then(() => console.log("LOADED")).catch((e) => console.log("CAUGHT: " + e));\n');
    const proc = Bun.spawnSync(
      [RUNNER, main, "--output=json", "--timeout=100"],
      { stdout: "pipe", stderr: "pipe", timeout: 10_000 },
    );
    const json = JSON.parse(proc.stdout.toString());
    if (proc.exitCode !== 1) throw new Error(`Dynamic-import timeout exit code should be 1, got ${proc.exitCode}`);
    if (json.error?.type !== "TimeoutError") throw new Error(`Expected TimeoutError, got ${json.error?.type}`);
    const printed = (json.stdout ?? "") + proc.stdout.toString();
    if (printed.includes("CAUGHT")) throw new Error(`Timeout was swallowed by JS .catch()`);
  } finally {
    clean(tmp);
  }
}

// -- --max-instructions (Loader: infinite loop) ---------------------------------

console.log("--max-instructions...");
{
  const loop = "const iterable = { [Symbol.iterator]: () => ({ next: () => ({ done: false, value: 1 }) }) }; for (const x of iterable) { }\n";
  const { exitCode, json } = runLoaderJson(loop, ["--max-instructions=500"], { timeout: 10_000 });
  if (exitCode !== 1) throw new Error(`Instruction limit exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "InstructionLimitError") throw new Error(`Expected InstructionLimitError, got ${json.error?.type}`);
}

// -- super-constructor resolution terminates without the sandbox limits --------
//
// ES2026 §13.3.7.3 GetSuperConstructor resolves super() through the
// constructor's [[Prototype]]. A resolver that fell back to the declared
// superclass when that hop landed on a non-constructor walked the union of two
// relations: each is acyclic on its own, their union is not. The union spun
// forever inside one native call, so neither --timeout nor --max-instructions
// could interrupt it. It has to terminate on its own.

const retargetCycle =
  "class Base { b = 1; }\n" +
  "class Middle extends Base { m = 2; }\n" +
  "class Leaf extends Middle { l = 3; }\n" +
  "Object.setPrototypeOf(Leaf, {});\n" +
  "Object.setPrototypeOf(Middle, Leaf);\n" +
  "new Leaf();\n";

{
  console.log("super-constructor cycle terminates...");
  const { exitCode, json } = runLoaderJson(retargetCycle, [], { timeout: 20_000 });
  if (exitCode !== 1) throw new Error(`Retarget cycle exit code should be 1, got ${exitCode}`);
  if (json.error?.type !== "TypeError") {
    throw new Error(`Expected TypeError for the retarget cycle, got ${json.error?.type}`);
  }
  // The instruction limit must not change the outcome for this program: it
  // terminates on its own, so the TypeError is reported either way.
  const bounded = runLoaderJson(retargetCycle, ["--max-instructions=5000000"], { timeout: 20_000 });
  if (bounded.json.error?.type !== "TypeError") {
    throw new Error(`Retarget cycle should still be a TypeError under a limit, got ${bounded.json.error?.type}`);
  }
}

// -- --max-memory (Loader) ------------------------------------------------------

console.log("--max-memory (default positive)...");
{
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--compat-asi"]);
  if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Default maxBytes should be positive, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (override)...");
{
  const { json } = runLoaderJson("Goccia.gc.maxBytes\n", ["--max-memory=5000000", "--compat-asi"]);
  if (json.files?.[0]?.result !== 5000000) throw new Error(`Override maxBytes should be 5000000, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (OOM triggers RangeError)...");
{
  // Leave room for the live heap, native startup storage and the result's
  // element slots. The native storage includes the engine's own atoms and
  // shapes, which count against the limit (about 1.5 MB at startup) but not
  // in bytesAllocated. The mapped element objects, about 48 bytes each past
  // their slots, exhaust the margin.
  const ELEMENTS = 200_000;
  const POINTER_BYTES = 8;
  const MARGIN_BYTES = 4 * 1024 * 1024;
  const probe = await $`echo 'Goccia.gc(); console.log(Goccia.gc.bytesAllocated);' | ${RUNNER} --compat-asi 2>/dev/null`.quiet().nothrow();
  const liveBytes = Number.parseInt(probe.text().trim().split("\n")[0], 10);
  if (probe.exitCode !== 0 || !Number.isFinite(liveBytes) || liveBytes <= 0) {
    throw new Error(`Could not measure the live heap: exit ${probe.exitCode}, ${probe.text()}`);
  }
  const maxMemory = liveBytes + ELEMENTS * POINTER_BYTES + MARGIN_BYTES;
  const res = await $`echo ${`Array.from({length:${ELEMENTS}},(_,i)=>{ if (i===0) console.log('mapper started'); return {x:i}; });`} | ${RUNNER} --max-memory=${maxMemory} --compat-asi 2>&1`.quiet().nothrow();
  const out = res.text();
  if (res.exitCode !== 1) throw new Error(`OOM exit code should be 1, got ${res.exitCode}`);
  if (!containsLine(out, "mapper started")) throw new Error(`OOM should occur during mapping: ${out}`);
  if (!out.includes("RangeError")) throw new Error(`OOM output should contain RangeError: ${out}`);
  if (out.includes("memory budget")) throw new Error(`OOM should be refused at the next safe point, not by a growth gate: ${out}`);
}

console.log("--max-memory (array copy storage is reserved before copying)...");
{
  const cases = {
    toSpliced: "Array.prototype.toSpliced.call(input, 0, 0)",
    toReversed: "Array.prototype.toReversed.call(input)",
    toSorted: "Array.prototype.toSorted.call(input)",
    with: "Array.prototype.with.call(input, 1, 0)",
    from: "Array.from(input)",
  };
  const dir = mkdtemp("goccia-array-copy-budget-");
  try {
    const path = join(dir, "copy.js");
    // The getter stops an unguarded copy at its first read. Test lengths on both
    // sides of the dense storage threshold without building a large sparse array.
    for (const length of [4 * 1024 * 1024, 64 * 1024 * 1024 + 1]) {
      for (const [name, copy] of Object.entries(cases)) {
        writeFileSync(path, `
const input = {
  length: ${length},
  get 0() { throw new Error('copy read an index'); },
  get [${length - 1}]() { throw new Error('copy read an index'); },
};
${copy};
`);
        const label = `${name} with ${length} slots`;
        const run = await runWithPeakRss([RUNNER, "--max-memory=16MiB", "--timeout=10000", path]);
        if (run.exitCode !== 1 || !run.output.includes("RangeError") ||
            !run.output.includes("Allocation failed: GC heap size exceeds the configured memory limit") ||
            run.output.includes("Error: copy read an index"))
          throw new Error(`${label} should be refused before the first indexed read: ${run.output}`);
        assertPeakRssBelow(run, label, 40 * 1024 * 1024);
      }
    }
  } finally {
    clean(dir);
  }
}

console.log("--max-memory (memory outside the heap is charged; sized allocations are refused before they are made)...");
{
  // Each script grows memory the collected heap did not count before #1173:
  // atoms and dictionary shapes for array indices past the dense limit and
  // for fresh string keys, permanent symbols, and the text buffers of native
  // string builders; or asks for one allocation larger than the limit (an
  // array's elements, a built string, a sparse copy). Each must be refused
  // with a catchable RangeError while the process stays within twice the
  // ceiling above an idle run: before the fix they peaked at 3 to 30 times it.
  const ceiling = 64 * 1024 * 1024;
  const cases: Record<string, string> = {
    "sparse fill": "new Array(2e8).fill(0);",
    "fresh string keys": "const o = {}; let i = 0; while (i < 5e7) { o['k' + i] = 0; i++; }",
    "symbols": "const a = []; let i = 0; while (i < 5e7) { a.push(Symbol()); i++; }",
    "dense holes": "new Array(6e7);",
    "length setter": "const a = []; a.length = 6e7;",
    "padEnd": "''.padEnd(5e8, 'ab');",
    "repeat": "'ab'.repeat(2.5e8);",
    "join": "new Array(2e8).join('ab');",
    "JSON.stringify": "JSON.stringify(new Array(1e8));",
    "replaceAll": "'a'.repeat(1e6).replaceAll('a', 'b'.repeat(500));",
    "sparse copy": "Array.prototype.toSpliced.call({ length: 7e7 }, 0, 0);",
    "repeated $' substitutions": "('a' + 'b'.repeat(1e6)).replace('a', \"$'\".repeat(50));",
    "escaped JSON scalar": "JSON.stringify('\\0'.repeat(1e7));",
    "replaceAll through its own callback": "const s = 'a'.repeat(1e6); let d = 0; const f = () => { d++; if (d < 20) s.replaceAll('a', f); throw new Error('stop'); }; s.replaceAll('a', f);",
  };
  const permanent = new Set(["sparse fill", "fresh string keys", "symbols"]);
  const dir = mkdtemp("goccia-native-charge-");
  try {
    const path = join(dir, "grow.js");
    // The bound is above an idle run of the same binary, so that it does not
    // depend on how much a platform's runtime and allocator hold before the
    // script starts: on Linux x64 the cases measure 1.0 to 1.5 times the
    // ceiling above an idle run's ~20 MB.
    writeFileSync(path, "console.log('idle');\n");
    const idleRun = await runWithPeakRss([RUNNER, `--max-memory=${ceiling}`, path]);
    if (idleRun.exitCode !== 0) throw new Error(`An idle run failed (exit ${idleRun.exitCode}):\n${idleRun.output}`);
    const bound = (idleRun.peakRssBytes ?? 0) + 2 * ceiling;
    for (const [label, body] of Object.entries(cases)) {
      writeFileSync(path, `let caught = 'none';\ntry {\n${body}\n} catch (e) { caught = e.name; }\nconsole.log('caught', caught);\n`);
      const run = await runWithPeakRss([RUNNER, `--max-memory=${ceiling}`, "--compat-while-loops", "--timeout=60000", path]);
      // Keys and symbols live as long as the engine: once they fill the
      // limit, the next collection refuses again, which can land after the
      // catch. Those runs may end with that refusal uncaught.
      const refusedAgain = permanent.has(label) && run.exitCode === 1 &&
        run.output.includes("Allocation failed: GC heap size exceeds the configured memory limit");
      if (!refusedAgain && (run.exitCode !== 0 || !run.output.includes("caught RangeError")))
        throw new Error(`${label} should be refused with a catchable RangeError: exit ${run.exitCode}, ${run.output}`);
      assertPeakRssBelow(run, `${label}, above an idle run`, bound);
    }
    // Retrying with fresh keys after each refusal: the keys live as long as
    // the engine, so a later refusal can come inside a catch handler, even the
    // first, and end the run, but the process stays bounded instead of growing
    // each round (8 rounds reached 1.65 GB at 16 MiB before #1173).
    writeFileSync(path, [
      "let base = 1e8; let r = 0;",
      "while (r < 8) {",
      "  try { const a = []; a.length = 4e9; let i = 0; while (i < 5e7) { a[base + i] = 0; i++; } } catch (e) { console.log('round', r, e.name); }",
      "  base += 5e7; r++;",
      "}",
      "console.log('done');",
      "",
    ].join("\n"));
    const retry = await runWithPeakRss([RUNNER, `--max-memory=${ceiling}`, "--compat-while-loops", "--timeout=60000", path]);
    if (![0, 1].includes(retry.exitCode) ||
        !retry.output.includes("Allocation failed: GC heap size exceeds the configured memory limit"))
      throw new Error(`Retries with fresh sparse keys should each be refused: exit ${retry.exitCode}, ${retry.output}`);
    assertPeakRssBelow(retry, "retry with fresh sparse keys, above an idle run", bound);
    // Freezing a wide object changes each attribute in turn, a new shape each
    // time and no heap cell: it completes, and the shapes it drops are
    // collected on the way instead of piling up (543 MB at 16 MiB before).
    writeFileSync(path, "const o = {}; for (const i of Array.from({ length: 4000 }, (_, i) => i)) o['p' + i] = i; Object.freeze(o); console.log('frozen', Object.isFrozen(o));\n");
    const freeze = await runWithPeakRss([RUNNER, `--max-memory=${ceiling}`, "--timeout=60000", path]);
    if (freeze.exitCode !== 0 || !freeze.output.includes("frozen true"))
      throw new Error(`Freezing a wide object should complete: exit ${freeze.exitCode}, ${freeze.output}`);
    assertPeakRssBelow(freeze, "freezing a wide object, above an idle run", bound);
    // A refusal made before the allocation carries the suggestion the refusal
    // at a safe point does.
    writeFileSync(path, "''.padEnd(5e8, 'ab');\n");
    const early = Bun.spawnSync([RUNNER, `--max-memory=${ceiling}`, path], { stdout: "pipe", stderr: "pipe", timeout: 60_000 });
    const earlyOut = early.stdout.toString() + early.stderr.toString();
    if (early.exitCode !== 1 || !earlyOut.includes("Allocation failed: GC heap size exceeds the configured memory limit") ||
        !earlyOut.includes("Suggestion: call Goccia.gc() to free unreachable objects, or increase the limit with --max-memory"))
      throw new Error(`An early refusal should print the memory limit's suggestion: exit ${early.exitCode}, ${earlyOut}`);
  } finally {
    clean(dir);
  }
}

console.log("--max-memory (Blob bytes are charged and given back)...");
{
  // A Blob holds its bytes natively, reserved against the limit for its life
  // (Goccia.Values.BlobValue). Over the limit, new Blob is the catchable
  // RangeError, measured before the bytes are allocated, so a part repeated
  // many times cannot build a native buffer past the limit first, and a
  // repeated string is charged by the bytes it adds, not once more per
  // element. The list of parts is charged as it grows, so an iterator that
  // never ends is refused by the limit too. Blobs that become garbage give
  // their reservation back, so many of them in turn fit under one limit.
  const source = [
    "const chunk = new Uint8Array(1 << 20);",
    "const one = new Blob([chunk]);",
    "const out = [];",
    "const tryIt = (fn) => { try { fn(); out.push('ok'); } catch (e) { out.push(e.constructor.name); } };",
    "tryIt(() => new Blob(Array(200).fill(chunk)));",
    "tryIt(() => new Blob(Array(1000).fill(one)));",
    "const s = 'x'.repeat(1 << 20);",
    "tryIt(() => new Blob(Array(500).fill(s)));",
    "tryIt(() => new Blob(Array(25).fill(s)));",
    "let total = 0;",
    "for (const _ of Array(300).keys()) { total += new Blob([chunk]).slice(1).size; }",
    "out.push(total);",
    "console.log(out.join(','));",
    "",
  ].join("\n");
  const res = await $`echo ${source} | ${RUNNER} --max-memory=67108864 --timeout=60000 2>&1`.quiet().nothrow();
  const out = res.text();
  if (res.exitCode !== 0) throw new Error(`Blob memory probe should exit 0, got ${res.exitCode}: ${out}`);
  if (!out.includes(`RangeError,RangeError,RangeError,ok,${300 * ((1 << 20) - 1)}`)) {
    throw new Error(`Blob memory probe: over the limit should be RangeError and garbage Blobs should fit in turn: ${out}`);
  }
  // The endless iterator runs under a smaller limit, which it reaches sooner.
  const endless = [
    "const empty = new Blob();",
    "try { new Blob({ [Symbol.iterator]: () => ({ next: () => ({ done: false, value: empty }) }) }); console.log('ok'); }",
    "catch (e) { console.log(e.constructor.name); }",
    "",
  ].join("\n");
  const endlessRes = await $`echo ${endless} | ${RUNNER} --max-memory=16777216 --timeout=60000 2>&1`.quiet().nothrow();
  if (endlessRes.exitCode !== 0 || !endlessRes.text().includes("RangeError")) {
    throw new Error(`Blob memory probe: an endless iterator of parts should be refused by the limit: ${endlessRes.text()}`);
  }
}

console.log("--max-memory (compiled RegExp storage shares the allocation budget)...");
{
  const dir = mkdtemp("goccia-regexp-budget-");
  const pattern = "a".repeat(32 * 1024);
  const expressions = {
    control: `pattern === ${JSON.stringify(pattern)}`,
    constructor: "new RegExp(pattern)",
    literal: `/${pattern}/`,
  };
  try {
    const path = join(dir, "regexp.js");
    // The room left with no compiled RegExp, measured: the largest buffer that
    // fits, to 16 KiB. The engine's own keys and shapes count against the
    // limit beside bytesAllocated, so it is not read from that.
    writeFileSync(path, `
const pattern = ${JSON.stringify(pattern)};
if (!(${expressions.control})) throw new Error('expected a match');
Goccia.gc();
let lo = 0;
let hi = Goccia.gc.maxBytes;
for (const _ of Array.from({ length: 40 })) {
  if (hi - lo <= 16 * 1024) break;
  const mid = Math.floor((lo + hi) / 2);
  try { new ArrayBuffer(mid); lo = mid; } catch { hi = mid; }
}
console.log('fits', lo);
`);
    const control = Bun.spawnSync([RUNNER, "--max-memory=16MiB", "--timeout=10000", path], { stdout: "pipe", stderr: "pipe" });
    const controlOut = control.stdout.toString() + control.stderr.toString();
    const fits = Number(/^fits (\d+)$/m.exec(controlOut)?.[1]);
    if (control.exitCode !== 0 || !Number.isFinite(fits) || fits <= 0)
      throw new Error(`The control run should measure the room left: ${controlOut}`);
    // A buffer that fits without a compiled RegExp is refused while a live
    // RegExp keeps one cached, before its bytes are committed. (A RegExp the
    // script dropped frees its pattern at the next major collection:
    // tests/built-ins/Goccia/gc/native-memory/regexp-patterns.js.)
    for (const [name, regexp] of Object.entries(expressions)) {
      if (name === "control") continue;
      writeFileSync(path, `
const pattern = ${JSON.stringify(pattern)};
const re = ${regexp};
if (!re.test(pattern)) throw new Error('expected a match');
Goccia.gc();
new ArrayBuffer(${fits});
console.log('buffer allocated', re.source.length);
`);
      const run = await runWithPeakRss([RUNNER, "--max-memory=16MiB", "--timeout=10000", path]);
      if (run.exitCode !== 1 || !run.output.includes("RangeError") || !run.output.includes("memory limit of"))
        throw new Error(`The buffer should be refused while the ${name} program is cached: ${run.output}`);
      assertPeakRssBelow(run, `RegExp ${name} budget refusal`, 40 * 1024 * 1024);
    }
    const operations = {
      compile: 'const re = new RegExp("a".repeat(64 * 1024)); console.log(re.source.length);',
      "compile literal": `console.log(/${"a".repeat(64 * 1024)}/.source.length);`,
      match: 'const re = new RegExp("a".repeat(256)); console.log(re.test("a".repeat(48 * 1024)));',
    };
    // The padding leaves 1 MiB of what fits (measured: the engine's own keys
    // and shapes count against the limit too) for the RegExp's workspace. A
    // literal is compiled with the script, before the padding exists, so its
    // run takes a limit its compile alone cannot fit under.
    const limits: Record<string, string> = { compile: "4MiB", "compile literal": "2MiB", match: "4MiB" };
    for (const [name, operation] of Object.entries(operations)) {
      writeFileSync(path, `
let lo = 0;
let hi = Goccia.gc.maxBytes;
for (const _ of Array.from({ length: 40 })) {
  if (hi - lo <= 16 * 1024) break;
  const mid = Math.floor((lo + hi) / 2);
  try { new ArrayBuffer(mid); lo = mid; } catch { hi = mid; }
}
const padding = new ArrayBuffer(Math.max(lo - 1024 * 1024, 0));
${operation}
console.log(padding.byteLength);
`);
      const run = await runWithPeakRss([RUNNER, `--max-memory=${limits[name]}`, "--timeout=10000", path]);
      if (run.exitCode !== 1 || !run.output.includes("RangeError") ||
          !run.output.includes("Allocation failed: GC heap size exceeds the configured memory limit"))
        throw new Error(`RegExp ${name} should report a memory-limit RangeError: ${run.output}`);
      assertPeakRssBelow(run, `RegExp ${name} workspace refusal`, 40 * 1024 * 1024);
    }
  } finally {
    clean(dir);
  }
}

console.log("--max-memory (a match's storage collects garbage before it is refused)...");
{
  // The heap is full of dead arrays when the match starts. A match reads its
  // subject in place and cannot collect, so its backtracking storage stops
  // the match, the engine collects, and the match runs again. Measured at
  // 8 MiB: without the collection the match is refused from 35,000 repetitions
  // up to 80,000; with it, it matches up to 60,000.
  const dir = mkdtemp("goccia-regexp-growth-");
  try {
    const path = join(dir, "growth.js");
    writeFileSync(path, `
const subject = "ab".repeat(45000);
const re = /^(a|ab)*$/;
let keep = null;
for (const i of Array.from({ length: 300 }, (_, k) => k)) keep = new Array(4000).fill(i);
keep = null;
console.log(re.test(subject));
`);
    const run = Bun.spawnSync([RUNNER, "--max-memory=8MiB", "--timeout=20000", path], { stdout: "pipe", stderr: "pipe" });
    const output = run.stdout.toString() + run.stderr.toString();
    if (run.exitCode !== 0 || !/^true$/m.test(output))
      throw new Error(`A match on a heap full of garbage should collect and match: exit ${run.exitCode}: ${output}`);
  } finally {
    clean(dir);
  }
}

console.log("--max-memory (peak resident memory stays near the ceiling)...");
if (process.platform === "linux") {
  // BytesAllocated charges each value its InstanceSize, well under half of
  // what the heap manager actually hands out, so a ceiling compared only
  // against it let collectable garbage grow to about six times the ceiling
  // before a collection ran (#1442). The collector now also triggers on the
  // heap manager's in-use total (ADR 0129). Both scripts keep little alive and
  // churn garbage far past the ceiling: a flat loop of short-lived objects and
  // arrays, and Promise jobs drained in batches. Linux only.
  //
  // The third drops 300 arrays made by `new Array(100000)`: about two
  // registrations for 800 KB of element storage each, which is gated but not
  // charged. With the heap sampled only every 1,024 registrations it reached
  // 340 MiB resident with no collection (#1747); the gated growth now samples
  // the heap as it is reported (ADR 0145).
  const ceiling = 64 * 1024 * 1024;
  // Bun's resourceUsage.maxRSS is getrusage's ru_maxrss, which Linux reports
  // in KiB; Bun 1.3 passes that through and Bun 1.4 converts it to bytes. The
  // two readings cannot be confused for these runs: the runner alone is
  // resident for more than 8 MB, and neither script comes near 8 GB (8 million
  // KiB) even with no collection at all.
  const peakBytes = (maxRSS: number) => (maxRSS < 8_000_000 ? maxRSS * 1024 : maxRSS);
  const scripts = {
    flat: [
      "let n = 0;",
      "for (const _ of Array.from({ length: 250000 })) { n += Reflect.ownKeys({ x: 1 }).length; }",
      "console.log('n', n);",
      "",
    ].join("\n"),
    promise: [
      "const N = 10000;",
      "const noop = () => {};",
      "const idx = Array.from({ length: N }, (_, i) => i);",
      "const time = async (fn) => { for (const _ of [0, 1, 2, 3, 4]) { idx.forEach(fn); await null; } };",
      "await time((i) => { Promise.resolve(i).then(noop); });",
      "await time((i) => { Promise.reject(i).catch(noop); });",
      "const thrower = async () => { throw 1; }; await time(() => { thrower().catch(noop); });",
      "const ok = async () => 1; await time(() => { ok().then(noop); });",
      "console.log('n', idx.length * 25);",
      "",
    ].join("\n"),
    holes: [
      "let last = null;",
      "for (const _ of Array.from({ length: 300 })) { last = new Array(100000); }",
      "console.log('n', last.length);",
      "",
    ].join("\n"),
  };
  const expected = { flat: "n 250000", promise: "n 250000", holes: "n 100000" };
  const dir = mkdtemp("goccia-rss-");
  try {
    for (const [name, src] of Object.entries(scripts)) {
      const path = join(dir, `${name}.js`);
      writeFileSync(path, src);
      const proc = Bun.spawnSync([RUNNER, path, `--max-memory=${ceiling}`], {

        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0 || !out.includes(expected[name as keyof typeof expected]))
        throw new Error(`${name} at --max-memory=${ceiling} should complete, got exit ${proc.exitCode}: ${out}`);
      const maxRSS = proc.resourceUsage?.maxRSS;
      if (typeof maxRSS !== "number" || maxRSS <= 0)
        throw new Error(`${name}: Bun reported no peak RSS for the child`);
      if (peakBytes(maxRSS) >= 2 * ceiling)
        throw new Error(`${name} peaked at ${peakBytes(maxRSS)} bytes resident, at or above twice the ${ceiling}-byte ceiling`);
    }
  } finally {
    clean(dir);
  }
} else {
  console.log("  skipped: peak RSS is asserted on Linux only");
}

console.log("--max-memory (a small live set collects as the heap grows under a far ceiling)...");
{
  // #1749: both ceiling triggers sit one pressure reserve below the ceiling,
  // so under a large ceiling a script that keeps 1,000 small objects alive
  // grew towards it before its first collection: no collection and 1.6 GiB
  // resident at 2 GiB. The heap trigger now also fires once the heap has
  // grown to four times what the last collection left, and by at least
  // 64 MiB (ADR 0145). This shorter run is checked for collections
  // everywhere and for peak resident memory on Linux, where Bun reports it:
  // before the growth trigger it made no collection and peaked near
  // 490 MiB, with it 6 collections and about 82 MiB.
  const src = [
    "const outer = Array.from({ length: 600 }, (_, k) => k);",
    "const inner = Array.from({ length: 1000 }, (_, k) => k);",
    "let batch = [];",
    "for (const i of outer) {",
    "  batch = [];",
    "  for (const j of inner) batch.push({ s: 'x' + (i * 1000 + j) });",
    "}",
    "console.log('kept', batch.length);",
    "",
  ].join("\n");
  const MAX_PEAK_BYTES = 150 * 1024 * 1024;
  const dir = mkdtemp("goccia-growth-");
  try {
    const path = join(dir, "growth.js");
    writeFileSync(path, src);
    const proc = Bun.spawnSync([RUNNER, path, "--max-memory=2GiB", "--output=json"], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 120_000,
    });
    const out = proc.stdout.toString();
    if (proc.exitCode !== 0 || !out.includes("kept 1000"))
      throw new Error(`Growth run at --max-memory=2GiB should complete, got exit ${proc.exitCode}: ${out}${proc.stderr.toString()}`);
    const collections = JSON.parse(out).memory?.gc?.collections;
    if (typeof collections !== "number" || collections < 1)
      throw new Error(`A small live set under a far ceiling should collect as the heap grows, got ${collections} collections`);
    if (process.platform === "linux") {
      const maxRSS = proc.resourceUsage?.maxRSS;
      if (typeof maxRSS !== "number" || maxRSS <= 0)
        throw new Error("Growth run: Bun reported no peak RSS for the child");
      // ru_maxrss in KiB under Bun 1.3, bytes under Bun 1.4; see above.
      const peak = maxRSS < 8_000_000 ? maxRSS * 1024 : maxRSS;
      if (peak >= MAX_PEAK_BYTES)
        throw new Error(`Growth run peaked at ${peak} bytes resident, at or above ${MAX_PEAK_BYTES}`);
    }
  } finally {
    clean(dir);
  }
}

console.log("--max-memory (own-key enumeration survives a mid-loop collection)...");
{
  // Enumerating a large property map allocates one string per key, and every
  // string allocation is charged against the ceiling — so it is a GC safe
  // point. The half-built result array must stay rooted across the loop or the
  // collection sweeps it and the builtin writes through a dangling pointer
  // (bus error / nil dereference, depending on build flags).
  const src = [
    "const outer = Array.from({ length: 30 }, (_, i) => i);",
    "const inner = Array.from({ length: 1000 }, (_, i) => i);",
    "const o = {};",
    "for (const a of outer) { for (const b of inner) { o['k' + (a * 1000 + b)] = b; } }",
    "const total = Object.keys(o).length + Object.values(o).length + Object.entries(o).length +",
    "  Object.getOwnPropertyNames(o).length + Reflect.ownKeys(o).length;",
    "console.log('total', total);",
    "total;",
    "",
  ].join("\n");

  // The window in which a collection lands mid-enumeration moves with the
  // build's object sizes, so sweep limits rather than pinning one value.
  for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608]) {
    const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, "--compat-asi"], {
      stdin: new TextEncoder().encode(src),
      stdout: "pipe",
      stderr: "pipe",
      timeout: 60_000,
    });
    const out = proc.stdout.toString() + proc.stderr.toString();
    // The engine has two memory limiters and they surface differently, which is
    // why the clauses below excuse one text and reject every other fatal. A
    // charged allocation (string payload, buffer) raises a script-catchable
    // RangeError. The growth gate (RequireNativeBytes, Goccia.MemoryLimit.pas)
    // raises TGocciaMemoryLimitError, which is deliberately opaque to the guest:
    // every boundary re-raises it, it escapes to the host, and the loader reports
    // it as "Fatal error: ... would exceed the memory budget" with exit 1. That
    // text is a refusal by design, not a crash — do not tighten these clauses
    // into failures. Any other fatal — bus error, access violation, nil object
    // check — is the heap corruption this test guards against.
    if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} crashed: ${out}`);
    if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
    // A run that completes under pressure must also have enumerated
    // correctly — a wrong count here is silent heap corruption.
    if (proc.exitCode === 0 && !out.includes("total 150000"))
      throw new Error(`Own-key enumeration at --max-memory=${maxMemory} completed with wrong total: ${out}`);
  }

  // With headroom the enumeration must complete and return every key.
  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=134217728", "--compat-asi"], { timeout: 60_000 });
  if (exitCode !== 0) throw new Error(`Own-key enumeration exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== 150000) throw new Error(`Own-key enumeration should return 150000, got ${json.files?.[0]?.result}`);
}

// Parking is measured, never assumed. A fixed ballast cap fails silently in
// both directions: at a tight ceiling the loop stops while the budget still has
// room to spare, and at a wide one the cap runs out long before the ceiling is
// in reach — either way the probe runs unparked and passes without ever entering
// the window it exists to test. This grows ballast in bounded passes, collecting
// between them so each measurement counts live bytes only, and reports the
// outcome for the assertions to insist on.
//
// Shared by every parked-heap block below (parser probes, parse ceiling, parse
// gate, stringify gate) so the calibration is defined once: they differ only in
// the slack they park at, which is measured per block and passed in here.
const parkingPreamble = (slackTarget: number): string[] => [
  `const SLACK = ${slackTarget};`,
  // Sized from the ceiling so it cannot run out, and materialised before the
  // first measurement so it counts as baseline live set instead of quietly
  // defeating the parking it is driving.
  "const iters = Array.from({ length: Math.ceil(Goccia.gc.maxBytes / 4096) + 64 }, (_, j) => j);",
  "const ballast = [];",
  "let slack = Goccia.gc.maxBytes;",
  "Goccia.gc();",
  // Each pass adds only live ballast and then collects, so the post-collection
  // slack falls monotonically and a handful of passes converges. Measuring
  // before the collection is what let the old loop stop early: it was reading
  // garbage the next collection would hand straight back.
  //
  // The push threshold sits one ballast chunk below the target because each
  // collection hands back a little transient garbage: pushing to exactly SLACK
  // lets the post-collection measurement bounce back just above it and stall
  // there for every remaining pass (CI stalled 116 bytes short of a 600000
  // target this way — the baseline live set differs per platform, so the
  // convergence point does too). The pass cap is a generous termination bound,
  // not a calibration: parking breaks out early on the first pass that lands.
  "for (const pass of Array.from({ length: 32 }, (_, p) => p)) {",
  "  for (const i of iters) {",
  "    if (Goccia.gc.maxBytes - Goccia.gc.bytesAllocated <= SLACK - 8192) break;",
  '    ballast.push("x".repeat(4096));',
  "  }",
  "  Goccia.gc();",
  "  slack = Goccia.gc.maxBytes - Goccia.gc.bytesAllocated;",
  "  if (slack <= SLACK) break;",
  "}",
  // Every caller asserts on "parked true"; the trailing numbers are diagnostics
  // for when a calibration drifts and the assertion starts failing.
  'console.log("parked", slack <= SLACK, "slack", slack, "ballast", ballast.length);',
];

console.log("--max-memory (builtin result builders survive mid-build collections)...");
{
  // Same defect class as own-key enumeration: any builtin that fills a result
  // container across string allocations (each charged against the ceiling and
  // therefore a GC safe point) must keep that container rooted. This exercises
  // the CSV/TSV parsers and revivers, URLSearchParams, RegExp match arrays,
  // and the Intl resolved-options/parts builders under a sweep of limits.
  const src = [
    'import * as CSVNS from "goccia:csv"; const CSV = CSVNS.CSV ?? CSVNS;',
    'import * as TSVNS from "goccia:tsv"; const TSV = TSVNS.TSV ?? TSVNS;',
    'const rows = Array.from({ length: 3000 }, (_, i) => "a" + i + ",b" + i + ",c" + i).join("\\n");',
    'const parsedCsv = CSV.parse("h1,h2,h3\\n" + rows);',
    'const trows = Array.from({ length: 3000 }, (_, i) => "a" + i + "\\tb" + i).join("\\n");',
    'const params = new URLSearchParams(Array.from({ length: 2000 }, (_, i) => "k=v" + i).join("&"));',
    'const m = "x123y456z".match(/(?<a>\\d+)y(?<b>\\d+)/d);',
    "const total = parsedCsv.length +",
    '  CSV.parse("h1,h2,h3\\n" + rows, {}, (k, v) => v).length +',
    '  CSV.parseChunk("h1,h2,h3\\n" + rows, {}, 0, -1).values.length +',
    '  TSV.parse("h1\\th2\\n" + trows).length +',
    '  params.getAll("k").length + [...params.entries()].length +',
    "  m.indices.groups.b[0] +",
    '  Intl.getCanonicalLocales(["en-US", "de-DE"]).length +',
    '  new Intl.PluralRules("en").resolvedOptions().pluralCategories.length +',
    '  new Intl.ListFormat("en").formatToParts(["a", "b", "c"]).length;',
    "console.log('total', total);",
    "",
  ].join("\n");
  const tmp = mkdtemp("goccia-memroot-");
  const srcPath = join(tmp, "sweep.mjs");
  writeFileSync(srcPath, src);
  try {
    for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608, 67_108_864]) {
      const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // Budget-text fatal = the uncatchable growth gate refusing; see the
      // two-limiter note on the own-key enumeration sweep above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("total 16014"))
        throw new Error(`Builder sweep at --max-memory=${maxMemory} completed with wrong total: ${out}`);
    }

    // With headroom the builders must complete and produce the exact total —
    // an all-RangeError sweep would otherwise verify nothing.
    {
      const proc = Bun.spawnSync([RUNNER, "--max-memory=134217728", srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (proc.exitCode !== 0)
        throw new Error(`Builder sweep headroom run should exit 0, got ${proc.exitCode}: ${out}`);
      if (!out.includes("total 16014"))
        throw new Error(`Builder sweep headroom run should report total 16014: ${out}`);
    }

    // Second wave of the same defect class, in builders the first sweep does not
    // reach: the JSONL chunk-result object and its error object, the JSON5
    // reviver context/holder plus the replacer's partially built copies, and the
    // Promise.allSettled entries — each is filled across a string allocation or
    // a property write, and each of those is charged against the ceiling and so
    // is a collecting safe point.
    const wave2Src = [
      'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
      'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
      'const lines = Array.from({ length: 800 }, (_, i) => \'{"k":"v\' + i + \'","n":\' + i + \'}\').join("\\n");',
      'const chunk = JSONL.parseChunk(lines + "\\n");',
      // An unterminated record makes parseChunk build the SyntaxError object too.
      "const badChunk = JSONL.parseChunk('{\"k\":1}\\n{\"k\":\\n');",
      'const json5Text = "{" + Array.from({ length: 500 }, (_, i) => "k" + i + ": \'v" + i + "\'").join(", ") + "}";',
      "const revived = JSON5.parse(json5Text, (k, v) => v);",
      'const nested = JSON5.parse("[" + Array.from({ length: 400 }, (_, i) => "{a: " + i + "}").join(", ") + "]", (k, v) => v);',
      "const replaced = JSON5.stringify(nested, (k, v) => v);",
      "const sync = chunk.values.length + badChunk.values.length +",
      "  (badChunk.error === null ? 0 : 1) + Object.keys(revived).length +",
      "  nested.length + replaced.length;",
      // The inputs that reject carry a handler from the moment they exist.
      // Without one, the sweep's verdict depended on where the ceiling
      // landed, not on whether it surfaced as a RangeError. A refused
      // allocation inside allSettled's iteration is a catchable RangeError
      // (a refusal at the next safe point, docs/errors.md), so PerformPromiseAllSettled
      // ends abruptly and allSettled rejects with it (ES2026 §27.2.4.2 step
      // 8.b, IfAbruptRejectPromise). The inputs past that point never get
      // allSettled's handlers, and the runner fails a run with its oldest
      // unhandled rejection, which was one of those guest Errors ("Error:
      // e401" at 1.5 MiB), older than the RangeError. allSettled still
      // subscribes to every input it reaches, so its rejected entries are
      // still built.
      "const ignore = () => {};",
      "Promise.allSettled(Array.from({ length: 500 }, (_, i) => {",
      '  if (i % 2 === 0) return Promise.resolve("v" + i);',
      '  const rejected = Promise.reject(new Error("e" + i));',
      "  rejected.catch(ignore);",
      "  return rejected;",
      "})).then((rs) => {",
      "  console.log('total', sync + rs.length +",
      '    rs.filter((r) => r.status === "fulfilled").length +',
      '    rs.filter((r) => r.status === "rejected").length);',
      "});",
      "",
    ].join("\n");
    const wave2Path = join(tmp, "sweep-wave2.mjs");
    writeFileSync(wave2Path, wave2Src);
    let rangeErrorRefusals = 0;
    for (const maxMemory of [1_048_576, 1_572_864, 2_097_152, 3_145_728, 8_388_608, 67_108_864]) {
      const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, wave2Path], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 120_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // Budget-text fatal = the uncatchable growth gate refusing, as above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("total 5793"))
        throw new Error(`Wave-2 builder sweep at --max-memory=${maxMemory} completed with wrong total: ${out}`);
      if (proc.exitCode !== 0 && out.includes("RangeError")) rangeErrorRefusals++;
    }
    // The counterpart of the headroom run below: a sweep in which nothing is
    // refused says nothing about how a refusal surfaces.
    if (rangeErrorRefusals === 0)
      throw new Error(`Wave-2 builder sweep never refused with a RangeError, so it proved nothing about the ceiling`);

    // As above, one guaranteed-success run so an all-refusals sweep cannot
    // pass vacuously.
    const proc = Bun.spawnSync([RUNNER, "--max-memory=134217728", wave2Path], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 120_000,
    });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0)
      throw new Error(`Wave-2 builder sweep headroom run should exit 0, got ${proc.exitCode}: ${out}`);
    if (!out.includes("total 5793"))
      throw new Error(`Wave-2 builder sweep headroom run should report total 5793: ${out}`);

    // A ceiling sweep only lands in the collecting window by luck: the pressure
    // collection runs inside the allocation that would cross the ceiling, so the
    // heap has to already sit right below it. This shape parks it there on
    // purpose — ballast is grown until the remaining budget is a fixed slack,
    // after which nearly every builder allocation collects — and then keeps the
    // reviver/replacer results alive so a swept container's slot is reused (that
    // reuse is what turns the dangling pointer into an observable fault). With
    // the JSON5 holder/context/copy roots removed this faults within seconds
    // ("Object reference is Nil", "Invalid type cast"); with them it either
    // completes or refuses cleanly.
    const parkedSrc = [
      'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
      "const ballast = [];",
      "for (const i of Array.from({ length: 20000 }, (_, j) => j)) {",
      "  if (Goccia.gc.maxBytes - Goccia.gc.bytesAllocated <= 262144) break;",
      '  ballast.push("x".repeat(2048));',
      "}",
      "const kept = [];",
      "let n = 0;",
      "for (const i of Array.from({ length: 600 }, (_, j) => j)) {",
      "  const revived = JSON5.parse(\"{a: 1, b: 'two'}\", (k, v) => v);",
      "  const text = JSON5.stringify({ a: 1, b: [2, 3] }, (k, v) => v);",
      "  kept.push(revived, text);",
      "  n += Object.keys(revived).length + text.length;",
      "}",
      "console.log('parked', n, ballast.length > 0, kept.length);",
      "",
    ].join("\n");
    const parkedPath = join(tmp, "sweep-parked.mjs");
    writeFileSync(parkedPath, parkedSrc);
    for (const maxMemory of [2_097_152, 3_145_728, 4_194_304]) {
      const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, parkedPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 180_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      // A parked heap makes the growth gate far more likely to be the limiter
      // that fires, and its refusal is uncatchable by design: the budget text is
      // a legitimate outcome here, anything else fatal is not. See the
      // two-limiter note on the own-key enumeration sweep above.
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} crashed: ${out}`);
      if (proc.exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} failed without a clean refusal (exitCode=${proc.exitCode}): ${out}`);
      if (proc.exitCode === 0 && !out.includes("parked 9000 true 1200"))
        throw new Error(`Parked-heap builder run at --max-memory=${maxMemory} completed with wrong result: ${out}`);
    }

    // The same defect class in the recursive parsers, which build their trees in
    // plain Pascal fields and locals (JSON's visitor stack, YAML's key/value
    // locals and anchor map, JSONL's record accumulator, TOML's node tree) that
    // the collector cannot see. The shape below parks the heap right under the
    // ceiling with no other garbage available, so the parse crosses the ceiling
    // partway through and the only thing the pressure collection can free is the
    // in-progress tree itself — after which the parse keeps writing into swept
    // containers and the remaining values reuse their memory.
    //
    // The parse is wrapped in try/catch because a refused allocation surfaces as a
    // catchable JS RangeError. A crash used to be indistinguishable from a clean
    // refusal — both arrived as a caught SyntaxError, because the builtins
    // converted every exception out of the parser into one — so the assertions
    // read the whole line rather than just its shape.
    const USE_AFTER_FREE_SIGNATURES = [
      "Access violation",
      "Invalid type cast",
      "Object reference is Nil",
      "SIGSEGV",
      "Segmentation fault",
      "EAccessViolation",
      // FPC runtime errors for a nil-object call and an invalid typecast, as
      // reported when no handler formats them.
      "Runtime error 210",
      "Runtime error 216",
      "Runtime error 219",
      "Runtime error 204",
    ];

    // A refusal whose message is empty: the shape a blanket `on E: Exception`
    // handler produces when it relabels an engine failure, since the Pascal
    // Message of a thrown JS value is empty by construction. `SyntaxError: ` with
    // nothing after it is a resource ceiling the guest can mistake for bad input,
    // so it is a failure, not a refusal.
    const EMPTY_REFUSAL = /^refused \S+ ::\s*$/m;

    // `expect` is the exact line the parse must produce when it completes.
    // "ok " on its own proves nothing: a container that was swept and had its
    // memory reused almost always yields silently wrong data rather than a crash,
    // so every length and count is read back and compared.
    const parserProbes: Array<{
      name: string;
      setup: string[];
      parse: string;
      check: string;
      expect: string;
    }> = [
      {
        name: "json-flat",
        setup: [
          'const doc = "[" + Array.from({ length: 1200 }, (_, i) => \'"\' + S + i + \'"\').join(",") + "]";',
        ],
        parse: "JSON.parse(doc)",
        check: "'ok', b.length, b[0].length, b[600].length, b[1199].length",
        expect: "ok 1200 201 203 204",
      },
      {
        name: "json-nested",
        setup: [
          'const doc = "[" + Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + i + \'","b":["\' + S + \'","\' + S + \'"]}\').join(",") + "]";',
        ],
        parse: "JSON.parse(doc)",
        check: "'ok', b.length, b[0].a.length, b[399].b[1].length",
        expect: "ok 400 201 200",
      },
      {
        name: "json5",
        setup: [
          'import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;',
          'const doc = "[" + Array.from({ length: 1200 }, (_, i) => "\'" + S + i + "\'").join(",") + "]";',
        ],
        parse: "JSON5.parse(doc)",
        check: "'ok', b.length, b[0].length, b[600].length, b[1199].length",
        expect: "ok 1200 201 203 204",
      },
      {
        // The JSONL accumulator: one array collecting every parsed record while
        // each line's parse builds strings that can collect. Only the caller's
        // hand-off window was rooted, not the loop that fills it.
        name: "jsonl-chunk",
        setup: [
          'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
          'const doc = Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + \'","b":"\' + S + i + \'"}\').join("\\n") + "\\n";',
        ],
        parse: "JSONL.parseChunk(doc)",
        check: "'ok', b.values.length, b.values[0].a.length, b.values[399].b.length, b.done, b.error === null",
        expect: "ok 400 200 203 true true",
      },
      {
        // The same accumulator reached through the whole-input entry point, which
        // returns the array directly instead of a chunk record.
        name: "jsonl-parse",
        setup: [
          'import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;',
          'const doc = Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + \'","b":"\' + S + i + \'"}\').join("\\n") + "\\n";',
        ],
        parse: "JSONL.parse(doc)",
        check: "'ok', b.length, b[0].a.length, b[399].b.length",
        expect: "ok 400 200 203",
      },
      {
        name: "yaml-block-and-flow",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const doc = Array.from({ length: 300 }, (_, i) => "k" + i + ":\\n  a: \'" + S + i + "\'\\n  b: [\'" + S + "\', \'" + S + "\']\\n  c:\\n    - \'" + S + "\'\\n    - \'" + S + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.k0.a.length, b.k299.b[1].length, b.k299.c[1].length",
        expect: "ok 300 201 200 200",
      },
      {
        // Explicit `? key` / `: value` entries: the key survives a full nested
        // node parse before it is canonicalised, and the value survives the
        // canonicalisation. Long keys make both windows wide.
        name: "yaml-explicit-keys",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const K = "k".repeat(200);',
          'const doc = Array.from({ length: 200 }, (_, i) => "? " + K + i + "\\n:\\n  - \'" + S + "\'\\n  - \'" + S + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b[K + 0].length, b[K + 199][1].length",
        expect: "ok 200 2 200",
      },
      {
        // Anchors and aliases specifically: a value referenced only by the anchor
        // map is invisible to the collector without a root over that map.
        name: "yaml-anchors",
        setup: [
          'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
          'const doc = Array.from({ length: 250 }, (_, i) => "a" + i + ": &anc" + i + "\\n  x: \'" + S + i + "\'\\n  y: [\'" + S + "\', \'" + S + "\']\\nb" + i + ": *anc" + i + "\\nc" + i + ":\\n  <<: *anc" + i + "\\n  z: \'" + S + i + "\'").join("\\n") + "\\n";',
        ],
        parse: "YAML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.a0.x.length, b.b249.y[1].length, b.c249.z.length, b.c249.x.length",
        expect: "ok 750 201 200 203 203",
      },
      {
        name: "toml",
        setup: [
          'import * as TOMLNS from "goccia:toml"; const TOML = TOMLNS.TOML ?? TOMLNS;',
          'const doc = Array.from({ length: 300 }, (_, i) => "k" + i + \' = "\' + S + i + \'"\\narr\' + i + \' = ["\' + S + \'", "\' + S + \'"]\\ninl\' + i + \' = { x = "\' + S + \'", y = "\' + S + \'" }\').join("\\n") + "\\n";',
        ],
        parse: "TOML.parse(doc)",
        check: "'ok', Object.keys(b).length, b.k0.length, b.arr299[1].length, b.inl299.y.length",
        expect: "ok 900 201 200 200",
      },
    ];

    // Shared verdict for every parked-parse run. `requireParked` is off only for
    // the headroom run, which deliberately carries no ballast. `requireExpect`
    // is on for the calibrated window run, where a refusal is not an acceptable
    // outcome — see there.
    const assertProbeRun = (
      label: string,
      out: string,
      exitCode: number | null,
      expect: string,
      requireParked: boolean,
      requireExpect: boolean,
    ): void => {
      const signature = USE_AFTER_FREE_SIGNATURES.find((text) => out.includes(text));
      if (signature) throw new Error(`${label} hit a use-after-free (${signature}): ${out}`);
      if (out.includes("Fatal error") && !out.includes("would exceed the memory budget"))
        throw new Error(`${label} crashed: ${out}`);
      if (requireParked && !out.includes("parked true"))
        throw new Error(
          `${label} never got the heap under the ceiling, so it exercised no collecting window: ${out}`,
        );
      if (EMPTY_REFUSAL.test(out))
        throw new Error(
          `${label} refused with an empty message — an engine failure relabelled as a syntax error: ${out}`,
        );
      if (out.includes("refused ") && !out.includes("refused RangeError"))
        throw new Error(
          `${label} refused with something other than the RangeError a memory ceiling raises: ${out}`,
        );
      if (requireExpect) {
        // No `|| refused` escape: this run is calibrated to finish, and a
        // refusal means the calibration drifted rather than that the ceiling
        // did its job. Accepting one here would silently retire the only check
        // that reads the parsed values back under collection pressure.
        if (exitCode !== 0 || !out.includes(expect))
          throw new Error(
            `${label} did not complete with "${expect}" while parked (exitCode=${exitCode}): ${out}`,
          );
        return;
      }
      if (exitCode !== 0 && !out.includes("RangeError") && !out.includes("would exceed the memory budget"))
        throw new Error(`${label} failed without a clean refusal (exitCode=${exitCode}): ${out}`);
      if (exitCode === 0 && !out.includes(expect) && !out.includes("refused RangeError"))
        throw new Error(`${label} produced neither "${expect}" nor a clean refusal: ${out}`);
    };

    for (const probe of parserProbes) {
      const buildSrc = (preamble: string[]): string =>
        [
          ...probe.setup.filter((line) => line.startsWith("import ")),
          'const S = "s".repeat(200);',
          ...probe.setup.filter((line) => !line.startsWith("import ")),
          ...preamble,
          "try {",
          `  const b = ${probe.parse};`,
          `  console.log(${probe.check});`,
          "} catch (e) {",
          "  console.log('refused', e.name, '::', e.message);",
          "}",
          "",
        ].join("\n");

      // A sweep of ceilings across the parse. The limit refuses at the
      // next back edge or call after a full collection leaves the heap over it
      // (ADR 0185), so each run either completes, and every value has to read
      // back exactly, or ends in that RangeError; never a crash, a use after
      // free or an empty message. The old collector's parked-heap windows
      // measured its pressure reserve, which the engine does not have; its rooting
      // under collection is checked by GC stress (gc-stress.yml: every test
      // file with a 16 KB nursery and the heap verified after each collection).
      for (const maxMemory of [4_194_304, 6_291_456, 8_388_608, 12_582_912]) {
        const sweepPath = join(tmp, `parser-sweep-${probe.name}-${maxMemory}.mjs`);
        writeFileSync(sweepPath, buildSrc([]));
        const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, sweepPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        assertProbeRun(
          `Ceiling-sweep ${probe.name} parse at --max-memory=${maxMemory}`,
          proc.stdout.toString() + proc.stderr.toString(),
          proc.exitCode,
          probe.expect,
          false,
          false,
        );
      }

      // One run with room to spare and no ballast at all, so a probe that only
      // ever refuses cannot pass vacuously: the parse has to complete and every
      // value has to read back exactly.
      {
        const headroomPath = join(tmp, `parser-headroom-${probe.name}.mjs`);
        writeFileSync(headroomPath, buildSrc([]));
        const proc = Bun.spawnSync([RUNNER, "--max-memory=134217728", headroomPath], {
          stdout: "pipe",
          stderr: "pipe",
          timeout: 180_000,
        });
        const out = proc.stdout.toString() + proc.stderr.toString();
        if (proc.exitCode !== 0)
          throw new Error(`Headroom ${probe.name} run should exit 0, got ${proc.exitCode}: ${out}`);
        if (!out.includes(probe.expect))
          throw new Error(`Headroom ${probe.name} run should report "${probe.expect}": ${out}`);
      }
    }
  } finally {
    clean(tmp);
  }
}

// A refused allocation is a catchable RangeError at the next back edge
// or call (ADR 0185), so a script can drop what it holds and go on. A ceiling
// the guest can catch must still not be one it can ignore in a loop: a script
// that catches every refusal and keeps what it allocated ends, after eight
// refusals in a row that free nothing, with the uncatchable MemoryLimitError.
// Each shape below catches the refusal in a different place: the guest's own
// try/catch, an async function, a promise executor, Promise.all's and
// Promise.race's iteration, and an async generator's finally run by
// `for await ... break`.
{
  const hold = 'for (const j of Array(1000).keys()) keep.push({ j, s: "x" + j });';
  const loop = "for (const _ of Array(100000).keys())";
  const shapes: Array<[string, string]> = [
    ["sync try/catch", `const keep = []; ${loop} { try { ${hold} } catch (e) {} }\n`],
    [
      "async function body",
      `const keep = []; const grow = async () => { ${hold} }; const main = async () => { ${loop} { try { await grow(); } catch (e) {} } }; main();\n`,
    ],
    [
      "promise executor",
      `const keep = []; const main = async () => { ${loop} { try { await new Promise((r) => { ${hold} r(); }); } catch (e) {} } }; main();\n`,
    ],
    [
      "Promise.all iterator allocation",
      `const keep = []; const it = { [Symbol.iterator]: () => ({ next: () => { ${hold} return { done: true }; } }) }; const main = async () => { ${loop} { try { await Promise.all(it); } catch (e) {} } }; main();\n`,
    ],
    [
      "Promise.race iterator allocation",
      `const keep = []; let n = 0; const it = { [Symbol.iterator]: () => { n = 0; return { next: () => { ${hold} return n++ ? { done: true } : { done: false, value: 1 }; } }; } }; const main = async () => { ${loop} { try { await Promise.race(it); } catch (e) {} } }; main();\n`,
    ],
    [
      "async generator return/finally",
      `const keep = []; const obj = { async *g() { try { yield 1; } finally { ${hold} } } }; const main = async () => { ${loop} { try { for await (const v of obj.g()) { break; } } catch (e) {} } }; main();\n`,
    ],
  ];

  for (const [shape, src] of shapes) {
    console.log(`--max-memory (a refusal caught in a loop still ends the run: ${shape})...`);
    const { exitCode, json } = runLoaderJson(src, ["--max-memory=16777216"], { timeout: 120_000 });
    if (exitCode !== 1) throw new Error(`Memory limit refusal (${shape}) should exit 1, got ${exitCode}: ${JSON.stringify(json)}`);
    if (json.error?.type !== "MemoryLimitError") throw new Error(`Memory limit refusal (${shape}) should report MemoryLimitError, got ${json.error?.type}`);
  }
}

// The data-format parse builtins are the other half of that convention. Their
// handlers used to catch every Pascal exception out of the parser and re-throw it
// as a SyntaxError, which is the same swallow in a different disguise: the
// ceiling still reached the guest, but wearing a name that says "your input is
// malformed" and carrying no message at all (a thrown JS value's Pascal Message
// is empty by construction, so the conversion produced `SyntaxError: ` and
// nothing else). Each handler now names only its own parse-error class, so a
// refusal keeps its RangeError identity and its message.
//
// TOML is included even though its handler was already narrow — it is the
// reference shape the others were brought in line with, and it should stay that
// way.
{
  const ceilingTmp = mkdtemp("goccia-parse-ceiling-");
  try {
    const parkedParse: Array<[string, string[], string]> = [
      ["JSON.parse", [], "JSON.parse(doc)"],
      [
        "JSON5.parse",
        ['import * as JSON5NS from "goccia:json5"; const JSON5 = JSON5NS.JSON5 ?? JSON5NS;'],
        "JSON5.parse(doc)",
      ],
      [
        "YAML.parse",
        ['import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;'],
        'YAML.parse(Array.from({ length: 400 }, (_, i) => "k" + i + ": \'" + S + i + "\'").join("\\n") + "\\n")',
      ],
      [
        "JSONL.parse",
        ['import * as JSONLNS from "goccia:jsonl"; const JSONL = JSONLNS.JSONL ?? JSONLNS;'],
        'JSONL.parse(Array.from({ length: 400 }, (_, i) => \'{"a":"\' + S + i + \'"}\').join("\\n") + "\\n")',
      ],
      [
        "TOML.parse",
        ['import * as TOMLNS from "goccia:toml"; const TOML = TOMLNS.TOML ?? TOMLNS;'],
        'TOML.parse(Array.from({ length: 400 }, (_, i) => "k" + i + \' = "\' + S + i + \'"\').join("\\n") + "\\n")',
      ],
    ];

    for (const [label, imports, parse] of parkedParse) {
      console.log(`--max-memory (parse refusal keeps its RangeError: ${label})...`);
      const src = [
        ...imports,
        'const S = "s".repeat(200);',
        'const doc = "[" + Array.from({ length: 1200 }, (_, i) => \'"\' + S + i + \'"\').join(",") + "]";',
        "try {",
        `  const value = ${parse};`,
        '  console.log("completed", value === null || value === undefined ? "empty" : "value");',
        "} catch (e) {",
        "  console.log('caught', e.name, '::', e.message);",
        "}",
        "",
      ].join("\n");
      const srcPath = join(ceilingTmp, `parse-ceiling-${label.replace(".", "-")}.mjs`);
      writeFileSync(srcPath, src);
      // A sweep of ceilings: the refusal lands at the next back edge or
      // call (ADR 0185), so it may be caught around the parse, reach the host
      // after the parse finished, or not come at all; at no ceiling may it be a
      // SyntaxError or lose its message.
      for (const maxMemory of [2_097_152, 3_145_728, 4_194_304, 6_291_456]) {
      const proc = Bun.spawnSync([RUNNER, `--max-memory=${maxMemory}`, srcPath], {
        stdout: "pipe",
        stderr: "pipe",
        timeout: 180_000,
      });
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (/^caught \S+ ::\s*$/m.test(out))
        throw new Error(
          `Parse-ceiling ${label}
 surfaced a refusal with an empty message: ${out}`,
        );
      if (out.includes("caught SyntaxError"))
        throw new Error(
          `Parse-ceiling ${label}
 relabelled a memory ceiling as a SyntaxError: ${out}`,
        );
      if (!out.includes("RangeError") && !out.includes("completed"))
        throw new Error(
          `Parse-ceiling ${label}
 produced neither a RangeError refusal nor a completed parse: ${out}`,
        );
      }
    }
  } finally {
    clean(ceilingTmp);
  }
}

// An ordinary failing test file is still an ordinary failing test file. The
// runner now aborts the whole run — exit 70, no summary — when an engine
// integrity fault reaches one of its per-file arms (ADR 0109, "Host tier: the
// test runner"), and the failure mode worth guarding against is that policy
// widening to catch things it was never meant to. A failed assertion is a
// verdict on one file delivered by a sound heap: it stays exit 1, the passing
// file beside it still runs and still counts, and the summary is unchanged.
//
// The abort path itself is not reachable from here. Raising a genuine
// EObjectCheck inside a test file would take an injection hook in the runner —
// a switch whose only purpose is to corrupt a production binary on request —
// and that is not worth shipping for a test; the abort was verified by hand
// against a temporary build instead. What this block locks is the contract the
// abort must not disturb, in both --jobs shapes, since the sequential and
// parallel paths reach the arms differently.
console.log("TestRunner (a failing file stays exit 1, not an integrity abort)...");
{
  const failTmp = mkdtemp("goccia-runner-fail-");
  try {
    writeFileSync(
      join(failTmp, "failing.test.js"),
      'describe("d", () => {\n  test("fails", () => {\n    expect(1).toBe(2);\n  });\n});\n',
    );
    // The console.log marker proves the passing FILE executed — the count
    // assertions below prove one test passed, but only the marker ties that
    // pass to this file rather than to some shape change in the failing one.
    writeFileSync(
      join(failTmp, "passing.test.js"),
      'console.log("RAN: passing.test.js");\ndescribe("d", () => {\n  test("passes", () => {\n    expect(1).toBe(1);\n  });\n});\n',
    );
    for (const jobsArg of ["--jobs=1", "--jobs=2"]) {
      const label = jobsArg;
      const proc = Bun.spawnSync(
        [TESTRUNNER, failTmp, "--no-progress", jobsArg],

        { stdout: "pipe", stderr: "pipe" },
      );
      const out = proc.stdout.toString() + proc.stderr.toString();
      if (out.includes("Integrity fault:"))
        throw new Error(`TestRunner (${label}) treated a failed assertion as an integrity fault: ${out}`);
      if (proc.exitCode !== 1)
        throw new Error(`TestRunner (${label}) should exit 1 on a failed test, got ${proc.exitCode}: ${out}`);
      // The report shape the abort must leave alone: the failing file counted
      // once, the file beside it still executed, and the failure named.
      for (const expected of [
        "Test Results Test Files: 2",
        "Test Results Run Tests: 2",
        "Test Results Passed: 1 (50.00%)",
        "Test Results Failed: 1 (50.00%)",
        'Test "fails" in suite "d": Expected 1 to be 2',
        // The guest marker directly proves the passing FILE executed. The
        // parallel workers capture guest console output, so the marker is
        // only observable sequentially; the jobs=2 shape keeps the count
        // assertions, which pin the same fact arithmetically.
        ...(jobsArg === "--jobs=1" ? ["RAN: passing.test.js"] : []),
      ]) {
        if (!out.includes(expected))
          throw new Error(`TestRunner (${label}) report lost "${expected}": ${out}`);
      }
    }
  } finally {
    clean(failTmp);
  }
}

console.log("--max-memory (a reservation collects before it refuses)...");
{
  // An explicit reservation (an ArrayBuffer's bytes) is checked against the
  // ceiling before the allocation: when it does not fit, a full collection
  // runs and the test is made again, and only a reservation that still does
  // not fit is refused, at once, with a catchable RangeError
  // (docs/garbage-collector.md, "Gated growth points"). So garbage on the heap
  // never refuses a reservation that fits once it is gone, and a live set does.
  const reserveTmp = mkdtemp("goccia-reserve-collect-");
  try {
    const RESERVE_CEILING = 67_108_864;
    const reservationSource = (keepAlive: boolean): string =>
      [
        "let held = [];",
        'for (const j of Array.from({ length: 40 }, (_, i) => i)) held.push(new Array(125000).fill(j));',
        keepAlive ? "" : "held = null;",
        "try {",
        "  const buffer = new ArrayBuffer(40_000_000);",
        '  console.log("reserved", buffer.byteLength);',
        "} catch (e) {",
        "  console.log('refused', e.name, '::', e.message);",
        "}",
        'console.log("held", held === null ? 0 : held.length);',
        "",
      ].join("\n");

    // ~40 MB of arrays dropped before a 40 MB reservation: the heap with them
    // would not fit, the heap after a collection does.
    const garbagePath = join(reserveTmp, "reserve-garbage.mjs");
    writeFileSync(garbagePath, reservationSource(false));
    const garbageProc = Bun.spawnSync([RUNNER, `--max-memory=${RESERVE_CEILING}`, garbagePath], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 180_000,
    });
    const garbageOut = garbageProc.stdout.toString() + garbageProc.stderr.toString();
    if (garbageProc.exitCode !== 0 || !garbageOut.includes("reserved 40000000"))
      throw new Error(`A reservation that fits once the garbage is collected must succeed: ${garbageOut}`);

    // The same arrays kept alive: no collection makes room, so the reservation
    // is refused at once and the script goes on.
    const livePath = join(reserveTmp, "reserve-live.mjs");
    writeFileSync(livePath, reservationSource(true));
    const liveProc = Bun.spawnSync([RUNNER, `--max-memory=${RESERVE_CEILING}`, livePath], {
      stdout: "pipe",
      stderr: "pipe",
      timeout: 180_000,
    });
    const liveOut = liveProc.stdout.toString() + liveProc.stderr.toString();
    if (liveProc.exitCode !== 0 || !liveOut.includes("refused RangeError") || !liveOut.includes("held 40"))
      throw new Error(`A reservation a collection cannot make room for must be refused at once: ${liveOut}`);
  } finally {
    clean(reserveTmp);
  }
}

console.log("goccia:yaml (deep flow nesting reports a named, non-empty error)...");
{
  // Depth refusals out of the data-format parsers do not agree on a class, and
  // that split is pre-existing and deliberate here: JSON, JSON5 and JSONL hit
  // their own parser-internal nesting cap and report SyntaxError, while YAML and
  // TOML hit the shared native depth guard (EnterNativeDataDepth) and report
  // RangeError "Maximum call stack size exceeded". Documented, not changed.
  //
  // For YAML this is a reclassification the except-narrowing brought about: the
  // blanket handler used to relabel the depth guard's RangeError as a SyntaxError
  // with an empty message, so the guest could not tell a resource ceiling from
  // malformed input. The message being non-empty is the half that regressed.
  const depthTmp = mkdtemp("goccia-parse-depth-");
  try {
    const src = [
      'import * as YAMLNS from "goccia:yaml"; const YAML = YAMLNS.YAML ?? YAMLNS;',
      'const deep = "[".repeat(5000) + "]".repeat(5000);',
      "const report = (label, parse) => {",
      "  try {",
      "    parse();",
      '    console.log(label, "no-throw", "::", "no-throw");',
      "  } catch (e) {",
      "    console.log(label, e.name, '::', e.message);",
      "  }",
      "};",
      'report("yaml", () => YAML.parse(deep));',
      'report("json", () => JSON.parse(deep));',
      "",
    ].join("\n");
    const srcPath = join(depthTmp, "parse-depth.mjs");
    writeFileSync(srcPath, src);
    const proc = Bun.spawnSync([RUNNER, srcPath], { stdout: "pipe", stderr: "pipe", timeout: 60_000 });
    const out = proc.stdout.toString() + proc.stderr.toString();
    if (proc.exitCode !== 0) throw new Error(`Parse depth run should exit 0, got ${proc.exitCode}: ${out}`);
    if (!/^yaml RangeError :: .+$/m.test(out))
      throw new Error(`YAML deep flow nesting should report a named RangeError with a message: ${out}`);
    if (!out.includes("yaml RangeError :: Maximum call stack size exceeded"))
      throw new Error(`YAML deep flow nesting should report the native depth guard's message: ${out}`);
    // JSON.parse has no nesting limit of its own (ES2026 §25.5.1): an engine
    // may parse this depth, as V8 does, or refuse it with a named error.
    if (!/^json (no-throw :: no-throw|\w+Error :: .+)$/m.test(out))
      throw new Error(`JSON deep nesting should parse or report a named error with a message: ${out}`);
  } finally {
    clean(depthTmp);
  }
}

console.log("--max-memory (ordinary script errors stay catchable under a budget)...");
{
  // The counterweight: the same budget must not turn an in-language error
  // into a host-level failure.
  const src = "let caught = false; try { null.property; } catch (e) { caught = true; } caught\n";
  const { exitCode, json } = runLoaderJson(src, ["--max-memory=67108864", "--compat-asi"], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Catchable script error should exit 0, got ${exitCode}: ${JSON.stringify(json)}`);
  if (json.files?.[0]?.result !== true) throw new Error(`Catchable script error should be caught by the script, got ${json.files?.[0]?.result}`);
}

console.log("--max-memory (manual gc reclaims inside active calls)...");
{
  const src = [
    "const churn = (remaining) => remaining <= 0 ? Goccia.gc.bytesAllocated : (() => {",
    "  let junk = Array.from({ length: 300 }, (_, i) => ({ remaining, i }));",
    "  junk = null;",
    "  Goccia.gc();",
    "  return churn(remaining - 1);",
    "})();",
    "churn(30);",
    "",
  ].join("\n");

  // Include the now-charged native startup storage: about 1.7 MB with the
  // engine's own property keys and shapes. The collection count below still
  // requires all 30 manual collections during the recursive calls.
  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=2000000", "--compat-asi"], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Manual GC active-call exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (typeof json.files?.[0]?.result !== "number" || json.files[0].result <= 0) throw new Error(`Manual GC active-call should return positive bytesAllocated`);
  if ((json.memory?.gc?.collections ?? 0) < 30) throw new Error(`Manual GC active-call should report at least 30 collections, got ${json.memory?.gc?.collections}`);
}

console.log("--max-memory (loop pressure reclaims)...");
{
  const src = [
    "let total = 0;",
    "for (let round = 0; round < 1000; round++) {",
    "  let junk = Array.from({ length: 500 }, (_, i) => ({ round, i }));",
    "  total += junk.length;",
    "  junk = null;",
    "}",
    "total;",
    "",
  ].join("\n");
  const { exitCode, json, stderr } = runLoaderJson(src, [
    // Keep the pressure threshold below both 32-bit and 64-bit allocation
    // totals while leaving enough room for the loop's live object graph.
    "--max-memory=20000000",
    "--compat-asi",
    "--compat-traditional-for-loop",
  ], { timeout: 30_000 });
  if (exitCode !== 0) throw new Error(`Loop pressure exit code should be 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (json.files?.[0]?.result !== 500000) throw new Error(`Loop pressure should return 500000, got ${json.files?.[0]?.result}`);
  if ((json.memory?.gc?.collections ?? 0) <= 0) throw new Error(`Loop pressure should report collections, got ${json.memory?.gc?.collections}`);
}

console.log("--max-memory (a pressure collection marks deep live chains without native recursion)...");
{
  // The collector marked recursively, one native frame per object, so a
  // pressure collection taken while a deep chain was alive ran out of native
  // stack: a segmentation fault in a production build, here at the first
  // collection after the chains are built. It marks from an explicit mark
  // stack now (ADR 0147). The churn allocates far past the ceiling, so the
  // VM's checkpoints collect with both chains live; no Goccia.gc() call.
  const src = [
    "let head = { end: true };",
    "for (const _ of Array.from({ length: 300000 })) head = { child: head };",
    "class Base { base = 'base'; }",
    "let Leaf = Base;",
    "for (const _ of Array.from({ length: 30000 })) { const Parent = Leaf; Leaf = class extends Parent {}; }",
    "let n = 0;",
    "for (const _ of Array.from({ length: 1000 })) n += Array.from({ length: 500 }, (_, i) => ({ i })).length;",
    "let cur = head;",
    "let links = 0;",
    "for (const _ of Array.from({ length: 300000 })) { cur = cur.child; links++; }",
    "[links, cur.end, new Leaf().base, n];",
    "",
  ].join("\n");
  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=128MiB"], { timeout: 120_000 });
  if (exitCode !== 0)
    throw new Error(`Deep chains under pressure should exit 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (JSON.stringify(json.files?.[0]?.result) !== '[300000,true,"base",500000]')
    throw new Error(`Deep chains under pressure should survive every collection, got ${JSON.stringify(json.files?.[0]?.result)}`);
  if ((json.memory?.gc?.collections ?? 0) <= 0)
    throw new Error(`Deep chains under pressure should collect, got ${json.memory?.gc?.collections}`);
}

console.log("--max-memory (a live set inside the pressure reserve does not collect at every checkpoint)...");
{
  // #1673: the tracked-bytes trigger fires once BytesAllocated reaches the
  // pressure reserve below the ceiling (maxBytes / 8 = 8 MiB here). It used
  // to stay armed after a collection that could not get below it, so a
  // script whose working set fits but sits in that reserve ran a full
  // collection every 1,024 instructions and after every ArrayBuffer it made.
  // The trigger now backs off halfway from what survived to the ceiling, so
  // collections follow allocated bytes instead (ADR 0141).
  //
  // Counted, not timed. The ballast parks the live set 3 MiB under the
  // ceiling; the loops then allocate 20,000 small objects, one array
  // iterator per 1,000, and 500 small buffers. Before the back-off
  // this run took 779 collections, about one per 1,024 instructions in the
  // object loop and one per buffer; with it, 2.
  const src = [
    "const ballast = new ArrayBuffer(Goccia.gc.maxBytes - Goccia.gc.bytesAllocated - 3 * 1048576);",
    "const slack = Goccia.gc.maxBytes - Goccia.gc.bytesAllocated;",
    "const outer = Array.from({ length: 20 }, (_, i) => i);",
    "const inner = Array.from({ length: 1000 }, (_, i) => i);",
    "let last;",
    "for (const i of outer) for (const j of inner) last = { a: i, b: j };",
    "const buffers = Array.from({ length: 500 }, (_, i) => i);",
    "for (const i of buffers) last = new ArrayBuffer(64);",
    "const slackAfter = Goccia.gc.maxBytes - Goccia.gc.bytesAllocated;",
    "[slack < Goccia.gc.maxBytes / 8, slackAfter > 0, ballast.byteLength > 0];",
    "",
  ].join("\n");
  const MAX_COLLECTIONS = 40;
  const { exitCode, json, stderr } = runLoaderJson(src, ["--max-memory=64MiB"], { timeout: 120_000 });
  if (exitCode !== 0)
    throw new Error(`In-band allocation run should exit 0, got ${exitCode}: ${JSON.stringify(json)}${stderr}`);
  if (JSON.stringify(json.files?.[0]?.result) !== "[true,true,true]")
    throw new Error(
      `In-band allocation run should park inside the pressure reserve and stay under the ceiling, got ${JSON.stringify(json.files?.[0]?.result)}`,
    );
  const collections = json.memory?.gc?.collections;
  if (typeof collections !== "number" || collections > MAX_COLLECTIONS)
    throw new Error(
      `A live set inside the pressure reserve should collect at most ${MAX_COLLECTIONS} times for this allocation, got ${collections}`,
    );
}

console.log("--max-memory (maxBytes readonly)...");
{
  const res = await $`echo 'Goccia.gc.maxBytes = 999' | ${RUNNER} --compat-asi 2>&1`.nothrow();
  if (res.exitCode !== 1) throw new Error(`Read-only exit code should be 1, got ${res.exitCode}`);
  if (!res.text().includes("TypeError")) throw new Error(`Read-only should mention TypeError`);
}

// -- --max-stack (Loader) -------------------------------------------------------

console.log("--max-stack (default overflow)...");
{
  // A block body: a concise `() => f()` is a proper tail call and never overflows.
  const { exitCode, json } = runLoaderJson("const f = () => { f(); }; f();\n");
  if (exitCode !== 1) throw new Error(`Default overflow should exit 1, got ${exitCode}`);
  if (json.error?.type !== "RangeError") throw new Error(`Expected RangeError, got ${json.error?.type}`);
}

console.log("--max-stack (custom limit)...");
// The limit is the number of nested calls: n counts the calls that started,
// and the 101st is refused. The top level is not a call.
for (const sourceType of ["script", "module"]) {
  const out = await $`echo 'let n=0; const f=()=>{n++;f()}; try{f()}catch(e){console.log(n)};' | ${RUNNER} --max-stack=100 --source-type=${sourceType}`.text();
  if (!containsLine(out, "100"))
    throw new Error(`--max-stack=100 (${sourceType}) should allow 100 nested calls, got: ${out}`);
}

console.log("--max-stack (TestRunner test function)...");
// The runner calls the test function itself, so its calls have the whole limit.
{
  const src = [
    "const count = (k) => (k <= 1 ? 1 : count(k - 1) + 1);",
    'test("ten nested calls", () => { expect(count(10)).toBe(10); });',
    'test("eleven nested calls", () => { expect(() => count(11)).toThrow(RangeError); });',
  ].join("\n");
  const out = await $`echo ${src} | ${TESTRUNNER} --max-stack=10 --no-progress`.nothrow().text();
  if (!out.includes("Passed: 2"))
    throw new Error(`TestRunner --max-stack=10 should allow 10 nested calls in a test, got: ${out}`);
}

console.log("--max-stack (imported module top level)...");
// A module's top level is not a call, wherever it is imported from: each
// module counts its nested calls before the RangeError and prints them.
{
  const tmp = mkdtemp("goccia-max-stack-import-");
  try {
    const count = (label: string) =>
      `let n = 0; const f = () => { n++; f(); }; try { f(); } catch (e) { console.log("${label}", n, e.name); }\n`;
    writeFileSync(join(tmp, "dep.js"), `${count("dep")}export const dep = 1;\n`);
    writeFileSync(join(tmp, "inner.js"), `${count("inner")}export const inner = 1;\n`);
    writeFileSync(join(tmp, "outer.js"), `import { inner } from "./inner.js";\n${count("outer")}export const outer = 1;\n`);
    writeFileSync(join(tmp, "dynamic.js"), `${count("dynamic")}export const dynamic = 1;\n`);
    writeFileSync(
      join(tmp, "main.js"),
      `import { dep } from "./dep.js";\nimport { outer } from "./outer.js";\n${count("main")}await import("./dynamic.js");\n`,
    );
    const out = await $`${RUNNER} ${join(tmp, "main.js")} --max-stack=10`.nothrow().text();
    for (const label of ["dep", "inner", "outer", "main", "dynamic"]) {
      if (!containsLine(out, `${label} 10 RangeError`))
        throw new Error(`--max-stack=10: ${label}.js's top level should allow 10 nested calls, got: ${out}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("--max-stack (bytecode trampoline)...");
{
  const src = "let n = 0; const f = () => { n++; if (n < 20000) f(); }; f(); console.log(n);";
  const out = await $`echo ${src} | ${RUNNER} --max-stack=0`.text();
  if (!out.includes("20000")) throw new Error(`Trampoline should reach 20000, got: ${out}`);
}

// Constructors, async functions, generators, accessors, Proxy traps,
// ToPrimitive methods and callbacks from built-ins are entered by a native
// re-entry of the bytecode VM. Each is a nested call, and --max-stack counts
// it. Every script counts the calls that started in n and prints
// "RangeError <n>" once the limit stops it.
const nativeReentryShapes: Record<string, string> = {
  constructor: "class A { constructor() { n++; new A(); } } try { new A(); } catch (e) { console.log(e.name, n); }",
  "async function": "const f = async () => { n++; await f(); }; f().catch((e) => console.log(e.name, n));",
  "generator next()": "const o = { *g() { n++; o.g().next(); yield; } }; try { o.g().next(); } catch (e) { console.log(e.name, n); }",
  getter: "const o = { get x() { n++; return o.x; } }; try { o.x; } catch (e) { console.log(e.name, n); }",
  setter: "const o = { set x(v) { n++; o.x = v; } }; try { o.x = 1; } catch (e) { console.log(e.name, n); }",
  "Proxy trap": "const p = new Proxy({}, { get() { n++; return p.x; } }); try { p.x; } catch (e) { console.log(e.name, n); }",
  toString: "const o = { toString() { n++; return `${o}`; } }; try { `${o}`; } catch (e) { console.log(e.name, n); }",
  valueOf: "const o = { valueOf() { n++; return +o; } }; try { +o; } catch (e) { console.log(e.name, n); }",
  "iterator next()": "const it = { [Symbol.iterator]() { return { next() { n++; for (const x of it) {} return { done: true }; } }; } }; try { for (const x of it) {} } catch (e) { console.log(e.name, n); }",
  toJSON: "const o = { toJSON() { n++; return JSON.stringify(o); } }; try { JSON.stringify(o); } catch (e) { console.log(e.name, n); }",
  "sort comparator": "const c = () => { n++; [2, 1].sort(c); return 0; }; try { [2, 1].sort(c); } catch (e) { console.log(e.name, n); }",
  "Reflect.construct": "class A { constructor() { n++; Reflect.construct(A, []); } } try { new A(); } catch (e) { console.log(e.name, n); }",
  "yield*": "const o = { *g() { n++; yield* o.g(); } }; try { o.g().next(); } catch (e) { console.log(e.name, n); }",
  "map callback": "const f = () => { n++; [0].map(() => { f(); }); }; try { f(); } catch (e) { console.log(e.name, n); }",
};
const runNativeReentry = (shape: string, args: string[]) => {
  const run = Bun.spawnSync([RUNNER, "--max-memory=256MiB", ...args], {
    stdin: new TextEncoder().encode(`let n = 0; ${nativeReentryShapes[shape]}\n`),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 60_000,
  });
  return { exitCode: run.exitCode, out: run.stdout.toString() + run.stderr.toString() };
};

console.log("--max-stack (native re-entry counts as a call)...");
for (const shape of Object.keys(nativeReentryShapes)) {
  // Two calls per level, f and the callback map makes.
  const expected = shape === "map callback" ? "RangeError 5" : "RangeError 10";
  const { exitCode, out } = runNativeReentry(shape, ["--max-stack=10"]);
  if (exitCode !== 0 || !containsLine(out, expected))
    throw new Error(`--max-stack=10 should stop ${shape} recursion at "${expected}", got exit ${exitCode}: ${out}`);
}

console.log("--max-stack (native re-entry at the default limit)...");
{
  // 1,000 constructor calls fit in the default 2,200; a fixed cap of 512
  // native re-entries refused them.
  const src = 'let d = 0; class A { constructor(k) { d++; if (k) new A(k - 1); } } new A(999); console.log("ok", d);';
  const out = await $`echo ${src} | ${RUNNER}`.text();
  if (!containsLine(out, "ok 1000"))
    throw new Error(`1,000 nested constructor calls should fit the default limit, got: ${out}`);
}

console.log("--max-stack (a refused generator resume leaves the generator suspended)...");
{
  // At the limit the VM refuses to enter a generator's code, by the call
  // limit or by the native stack running low. The code has not run, so the
  // generator stays suspended and resumes later, instead of reporting that it
  // is already executing. return() and throw() on a generator that has not
  // started complete it without entering its code (ES2026 §27.5.3.4,
  // §27.6.1.3, §27.6.1.4), so the limit cannot refuse them. The work is
  // inline in the catch block: calling a helper there would itself be
  // refused.
  const resumeSync = 'try { fresh.next(); refused.push("ran"); } catch (x) { refused.push(x.name); } try { started.next(); refused.push("ran"); } catch (x) { refused.push(x.name); }' +
    ' abrupt.push(retGen.return(5)); try { thrGen.throw(boom); abrupt.push("ran"); } catch (x) { abrupt.push(x === boom ? "boom" : x.name); }';
  const resumeAsync = "asyncRefused = asyncFresh.next(); asyncAbrupt = [asyncRet.return(5), asyncThr.throw(boom)];";
  const atLimit = `if (!asyncRefused) { ${resumeSync} ${resumeAsync} }`;
  // How much native stack a call uses before the VM checks the headroom
  // depends on the native calls that lead to the check, and differs between
  // platforms. So the native-stack case descends through the same kind of
  // resume it then makes, from the same frame: a refused sync resume throws,
  // and a refused async generator request runs none of the callee's code.
  const nativeDescent = [
    "let entered = 0;",
    "const d = {",
    `  *sync() { try { d.sync().next(); } catch (e) { if (!refused.length) { ${resumeSync} } } },`,
    "  async *async() { entered++; const before = entered; const p = d.async().next();",
    `    if (entered === before && !asyncRefused) { p.catch(() => {}); ${resumeAsync} } },`,
    "};",
    "d.sync().next();",
    "d.async().next();",
  ].join("\n");
  const descents: Record<string, [string, string]> = {
    "call limit": [`const descend = () => { try { descend(); } catch (e) { ${atLimit} } }; descend();`, "--max-stack=50"],
    "native stack": [nativeDescent, "--max-stack=100000"],
  };
  for (const [label, [descend, limit]] of Object.entries(descents)) {
    const src = [
      "const o = { *g() { yield 1; yield 2; }, async *ag() { yield 1; yield 2; } };",
      "const fresh = o.g(), started = o.g(), asyncFresh = o.ag();",
      "const retGen = o.g(), thrGen = o.g(), asyncRet = o.ag(), asyncThr = o.ag();",
      'const boom = new Error("boom");',
      "started.next();",
      "const refused = [], abrupt = [];",
      "let asyncRefused, asyncAbrupt;",
      descend,
      'const rest = (it) => { try { return [...it].join(" "); } catch (e) { return e.name; } };',
      'asyncRefused.then(() => "ran", (e) => e.name).then(async (a) => {',
      '  let ar = []; try { for await (const v of asyncFresh) ar.push(v); ar = ar.join(" "); } catch (e) { ar = e.name; }',
      '  console.log(refused.join(","), "|", rest(fresh), "|", rest(started), "|", a, ar);',
      "  const fmt = (r) => `${r.value},${r.done}`;",
      "  const ra = fmt(await asyncAbrupt[0]), rn = fmt(await asyncRet.next());",
      '  const ta = await asyncAbrupt[1].then(() => "ran", (e) => (e === boom ? "boom" : e.name)), tn = fmt(await asyncThr.next());',
      '  console.log(fmt(abrupt[0]), fmt(retGen.next()), "|", abrupt[1], fmt(thrGen.next()), "|", ra, rn, "|", ta, tn);',
      "});",
      "",
    ].join("\n");
    const run = Bun.spawnSync([RUNNER, limit, "--max-memory=256MiB"], {
      stdin: new TextEncoder().encode(src),
      stdout: "pipe",
      stderr: "pipe",
      timeout: 60_000,
    });
    const out = run.stdout.toString() + run.stderr.toString();
    const expected = "RangeError,RangeError | 1 2 | 2 | RangeError 1 2";
    if (run.exitCode !== 0 || !containsLine(out, expected))
      throw new Error(`A generator resume refused at the ${label} should leave the generator suspended ("${expected}"), got exit ${run.exitCode}: ${out}`);
    const expectedAbrupt = "5,true undefined,true | boom undefined,true | 5,true undefined,true | boom undefined,true";
    if (!containsLine(out, expectedAbrupt))
      throw new Error(`return() and throw() at the ${label} should complete a generator that has not started ("${expectedAbrupt}"), got: ${out}`);
  }
}

console.log("--max-stack (native re-entry never overflows the native stack)...");
{
  // A limit far above what the native stack holds ends every shape in a
  // RangeError once the stack runs low, not in a crash.
  for (const shape of Object.keys(nativeReentryShapes)) {
    const { exitCode, out } = runNativeReentry(shape, ["--max-stack=100000"]);
    const count = /^RangeError (\d+)$/m.exec(out);
    if (exitCode !== 0 || !count || Number(count[1]) < 1000)
      throw new Error(`--max-stack=100000 should end ${shape} recursion in a RangeError, got exit ${exitCode}: ${out}`);
  }

  // The test runner runs files on worker threads, whose stacks it sizes
  // itself.
  const tmp = mkdtemp("goccia-native-reentry-");
  try {
    const endless: Record<string, string> = {
      "constructors.test.js": "class A { constructor() { new A(); } }\ntest('constructor', () => { expect(() => new A()).toThrow(RangeError); });\n",
      "async.test.js": "const f = async () => { await f(); };\ntest('async function', async () => { let caught; try { await f(); } catch (e) { caught = e; } expect(caught instanceof RangeError).toBe(true); });\n",
      "accessors.test.js": [
        "const o = { get x() { return o.x; }, set y(v) { o.y = v; } };",
        "const s = { *g() { s.g().next(); yield; } };",
        "test('getter', () => { expect(() => o.x).toThrow(RangeError); });",
        "test('setter', () => { expect(() => { o.y = 1; }).toThrow(RangeError); });",
        "test('generator next()', () => { expect(() => s.g().next()).toThrow(RangeError); });",
        "test('sort comparator', () => { const c = () => { [2, 1].sort(c); return 0; }; expect(() => [2, 1].sort(c)).toThrow(RangeError); });",
      ].join("\n") + "\n",
    };
    for (const [name, src] of Object.entries(endless)) writeFileSync(join(tmp, name), src);
    const res = await $`${TESTRUNNER} ${tmp} --max-stack=100000 --max-memory=256MiB --jobs=2 --no-progress`.nothrow().quiet();
    const out = res.text();
    if (res.exitCode !== 0 || !out.includes("Passed: 6"))
      throw new Error(`TestRunner --max-stack=100000 should end native re-entry recursion in a RangeError on worker threads, got exit ${res.exitCode}: ${out}`);
  } finally {
    clean(tmp);
  }
}

console.log("--max-stack (deep RangeError trace keeps 100 frames)...");
// A stack trace renders its innermost 100 frames and counts the rest, so the
// RangeError of a 200,000-deep recursion is as short as one of 101 frames.
// The frames are the 200,000 calls of f and the top level.
{
  const src =
    'let d = 0; const f = () => { d++; f(); }; try { f(); } catch (e) { const lines = e.stack.split("\\n"); console.log(lines.length, d, lines[lines.length - 1].trim()); }';
  const out = await $`echo ${src} | ${RUNNER} --max-stack=200000`.text();
  if (!containsLine(out, "102 200000 ... 199901 more frames"))
    throw new Error(`A 200,000-deep RangeError should keep 100 frames and count 199,901 more, got: ${out}`);
}

// -- Console observable behavior (Loader) ----------------------------------------


console.log("Console observable behavior...");
{
  const source = [
    'console.assert(true, "hidden");',
    'console.assert(false, "failed", 7);',
    'console.log("log", 1);',
    'console.warn("warn");',
    'console.error("error");',
    'console.info("info");',
    'console.debug("debug");',
    "console.dir({ value: 1 });",
    "console.count();",
    "console.count();",
    "console.countReset();",
    "console.count();",
    "let coercions = 0;",
    'console.count({ toString() { coercions++; return "coerced"; } });',
    'console.log("coercions", coercions);',
    'console.group("outer");',
    'console.log("inside");',
    'console.group("inner");',
    'console.trace("trace");',
    "console.table([1, 2]);",
    "console.groupEnd();",
    'console.error("outer");',
    "console.groupEnd();",
    "console.groupEnd();",
    'console.log("after");',
    "",
  ].join("\n");
  const expectedOutput = [
    "Assertion failed: failed 7",
    "log 1",
    "Warning: warn",
    "Error: error",
    "Info: info",
    "Debug: debug",
    "{ value: 1 }",
    "default: 1",
    "default: 2",
    "default: 1",
    "coerced: 1",
    "coercions 1",
    "outer",
    "  inside",
    "  inner",
    "    Trace: trace",
    "    [ 1, 2 ]",
    "  Error: outer",
    "after",
  ];
  const expectedStdout = [
    "log 1",
    "info",
    "debug",
    "{ value: 1 }",
    "default: 1",
    "default: 2",
    "default: 1",
    "coerced: 1",
    "coercions 1",
    "outer",
    "  inside",
    "  inner",
    "    [ 1, 2 ]",
    "after",
    "",
  ].join("\n");
  const expectedStderr = [
    "Assertion failed: failed 7",
    "warn",
    "error",
    "    trace",
    "  outer",
    "",
  ].join("\n");

  const { exitCode, json, stderr } = runLoaderJson(source);
  if (exitCode !== 0)
    throw new Error(`Console behavior failed: ${JSON.stringify(json.error)}${stderr}`);
  if (stderr !== "")
    throw new Error(
      `Console behavior leaked process stderr: ${JSON.stringify(stderr)}`,
    );
  if (JSON.stringify(json.output) !== JSON.stringify(expectedOutput))
    throw new Error(
      `Console behavior output mismatch: expected ${JSON.stringify(expectedOutput)}, got ${JSON.stringify(json.output)}`,
    );
  if (normalizeLineEndings(json.stdout) !== expectedStdout)
    throw new Error(
      `Console behavior stdout mismatch: expected ${JSON.stringify(expectedStdout)}, got ${JSON.stringify(json.stdout)}`,
    );
  if (normalizeLineEndings(json.stderr) !== expectedStderr)
    throw new Error(
      `Console behavior stderr mismatch: expected ${JSON.stringify(expectedStderr)}, got ${JSON.stringify(json.stderr)}`,
    );
}

console.log("Console timer lifecycle...");
{
  const source = [
    'console.timeEnd("missing");',
    'console.timeLog("missing-log");',
    "let coercions = 0;",
    'const label = { toString() { coercions++; return "timer"; } };',
    "console.time(label);",
    "console.timeLog(label);",
    "console.timeEnd(label);",
    "console.timeEnd(label);",
    'console.log("timer-label-coercions", coercions);',
    "console.time();",
    "console.timeEnd();",
    "console.timeEnd();",
    "",
  ].join("\n");

  const { exitCode, json, stderr } = runLoaderJson(source);
  if (exitCode !== 0)
    throw new Error(`Console timers failed: ${JSON.stringify(json.error)}${stderr}`);
  if (stderr !== "")
    throw new Error(
      `Console timers leaked process stderr: ${JSON.stringify(stderr)}`,
    );
  if (!Array.isArray(json.output) || json.output.length !== 8)
    throw new Error(`Console timers should emit 8 lines, got ${JSON.stringify(json.output)}`);

  const [missingEnd, missingLog, timeLog, timeEnd, removedEnd, coercions, defaultEnd, removedDefault] =
    json.output as string[];
  if (missingEnd !== "Timer 'missing' does not exist")
    throw new Error(`Console timers missing timeEnd output mismatch: ${missingEnd}`);
  if (missingLog !== "Timer 'missing-log' does not exist")
    throw new Error(`Console timers missing timeLog output mismatch: ${missingLog}`);
  if (!/^timer: \d+(?:\.\d{1,3})?ms$/.test(timeLog))
    throw new Error(`Console timers timeLog output mismatch: ${timeLog}`);
  if (!/^timer: \d+(?:\.\d{1,3})?ms$/.test(timeEnd))
    throw new Error(`Console timers timeEnd output mismatch: ${timeEnd}`);
  if (removedEnd !== "Timer 'timer' does not exist")
    throw new Error(`Console timers should remove an ended timer, got: ${removedEnd}`);
  if (coercions !== "timer-label-coercions 4")
    throw new Error(`Console timers should coerce each timer label once, got: ${coercions}`);
  if (!/^default: \d+(?:\.\d{1,3})?ms$/.test(defaultEnd))
    throw new Error(`Console timers default timer output mismatch: ${defaultEnd}`);
  if (removedDefault !== "Timer 'default' does not exist")
    throw new Error(`Console timers should remove the default timer, got: ${removedDefault}`);
  if (normalizeLineEndings(json.stderr) !== "")
    throw new Error(`Console timers should route output to stdout, got stderr: ${json.stderr}`);
}

// A host reading a piped GocciaRunner acts on console output mid-run: each
// line reaches the pipe as it is logged, and error, warn, assert and trace go
// to stderr, as in Node. The script logs, then fetches a loopback server whose
// handler answers only once both lines were read from the pipes, so an output
// held back until exit would fail the check rather than race it.
console.log("Console lines reach a piped stdout and stderr while the script runs...");
{
  const tmp = mkdtemp("goccia-console-pipes-");
  let stdoutText = "";
  let stderrText = "";
  let seenMidRun = false;
  const server = Bun.serve({
    hostname: "127.0.0.1",
    port: 0,
    async fetch() {
      const deadline = Date.now() + 15_000;
      while (Date.now() < deadline) {
        // Windows ends lines with CRLF; a line counts once it is complete.
        if (normalizeLineEndings(stdoutText).includes("running\n") &&
            normalizeLineEndings(stderrText).includes("Error: failing\n")) {
          seenMidRun = true;
          break;
        }
        await Bun.sleep(10);
      }
      return new Response("ok");
    },
  });
  try {
    const file = join(tmp, "pipes.js");
    writeFileSync(
      file,
      [
        // The stdout line comes last, so only its own flush can deliver it:
        // a stderr line flushes stdout first.
        'console.error("failing");',
        'console.log("running");',
        `await fetch("http://127.0.0.1:${server.port}/");`,
        'console.log("after");',
        "",
      ].join("\n"),
    );
    const proc = Bun.spawn([RUNNER, file, "--allow-net=127.0.0.1"], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const drain = async (stream: ReadableStream<Uint8Array>, append: (text: string) => void) => {
      const decoder = new TextDecoder();
      for await (const chunk of stream) append(decoder.decode(chunk, { stream: true }));
    };
    await Promise.all([
      drain(proc.stdout, (text) => { stdoutText += text; }),
      drain(proc.stderr, (text) => { stderrText += text; }),
      proc.exited,
    ]);
    stdoutText = normalizeLineEndings(stdoutText);
    stderrText = normalizeLineEndings(stderrText);
    if (proc.exitCode !== 0)
      throw new Error(`Piped console run should exit 0, got ${proc.exitCode}: ${stdoutText}${stderrText}`);
    if (!seenMidRun)
      throw new Error(`Console lines should reach the pipes before the script finishes, got stdout ${JSON.stringify(stdoutText)}, stderr ${JSON.stringify(stderrText)}`);
    if (!stdoutText.startsWith("running\nafter\n") || stdoutText.includes("failing"))
      throw new Error(`console.log lines belong on stdout and console.error lines do not, got ${JSON.stringify(stdoutText)}`);
    if (stderrText !== "Error: failing\n")
      throw new Error(`console.error should write to stderr, got ${JSON.stringify(stderrText)}`);
  } finally {
    server.stop(true);
    clean(tmp);
  }
}

// A console line written to a closed descriptor is dropped: it does not end
// the script, and a file opened later never takes the closed descriptor's
// place, so the --log file gets only its own records.
if (process.platform !== "win32") {
  console.log("Console output to a closed stderr is dropped without ending the script...");
  const tmp = mkdtemp("goccia-console-closed-");
  try {
    const file = join(tmp, "closed.js");
    const log = join(tmp, "run.log");
    writeFileSync(file, ['console.error("first");', 'console.error("second");', 'console.log("after");', ""].join("\n"));
    const proc = Bun.spawnSync(["sh", "-c", 'exec "$0" "$1" --log="$2" 2>&-', RUNNER, file, log], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const out = normalizeLineEndings(proc.stdout.toString());
    if (proc.exitCode !== 0 || !out.startsWith("after\n"))
      throw new Error(`console.error to a closed stderr should not end the script, got exit ${proc.exitCode}: ${out}`);
    const logged = normalizeLineEndings(readFileSync(log, "utf-8"));
    if (logged !== "[error] Error: first\n[error] Error: second\n[log] after\n")
      throw new Error(`The --log file should hold only its records with stderr closed, got ${JSON.stringify(logged)}`);
    // Descriptor 2 open for reading only: every write to it fails (EBADF)
    // and is dropped.
    const readOnly = Bun.spawnSync(["sh", "-c", 'exec "$0" "$1" 2</dev/null', RUNNER, file], {
      stdout: "pipe",
      stderr: "pipe",
    });
    const readOnlyOut = normalizeLineEndings(readOnly.stdout.toString());
    if (readOnly.exitCode !== 0 || !readOnlyOut.startsWith("after\n"))
      throw new Error(`console.error to a read-only stderr should not end the script, got exit ${readOnly.exitCode}: ${readOnlyOut}`);
    // stderr on a pipe whose reader has gone: the write raises SIGPIPE, which
    // the console holds off, so the script goes on and the runner exits 0,
    // with no tail of the failed line left to raise one at exit. The script
    // waits before it writes, so the reader (`:`) has exited by then; the
    // line is longer than one write so the RTL splits it.
    const late = join(tmp, "late.js");
    const lateOutPath = join(tmp, "late.out");
    const lateStatusPath = join(tmp, "late.status");
    writeFileSync(
      late,
      ["const start = Date.now();", "while (Date.now() - start < 500) {}", 'console.error("x".repeat(4000));', 'console.log("after");', ""].join("\n"),
    );
    const broken = Bun.spawnSync(
      ["sh", "-c", '{ "$0" "$1" --compat-while-loops 2>&1 >"$2"; echo "$?" >"$3"; } | :', RUNNER, late, lateOutPath, lateStatusPath],
      { stdout: "pipe", stderr: "pipe" },
    );
    const lateOut = normalizeLineEndings(readFileSync(lateOutPath, "utf-8"));
    const lateStatus = readFileSync(lateStatusPath, "utf-8").trim();
    if (lateStatus !== "0" || !lateOut.startsWith("after\n"))
      throw new Error(`console.error to a pipe with no reader should not end the script, got status ${lateStatus}, stdout ${JSON.stringify(lateOut)}: ${broken.stderr.toString()}`);
  } finally {
    clean(tmp);
  }
}

// With both streams on one file, the lines keep the order they were logged in.
console.log("Console lines keep their order with stdout and stderr on one file...");
{
  const tmp = mkdtemp("goccia-console-merged-");
  try {
    const file = join(tmp, "merged.js");
    writeFileSync(
      file,
      ['console.log("one");', 'console.error("two");', 'console.log("three");', 'console.warn("four");', ""].join("\n"),
    );
    const merged = await $`${RUNNER} ${file} 2>&1`.nothrow().text();
    const lines = normalizeLineEndings(merged).split("\n").slice(0, 4);
    if (JSON.stringify(lines) !== JSON.stringify(["one", "Error: two", "three", "Warning: four"]))
      throw new Error(`Merged console streams should keep their order, got ${JSON.stringify(merged)}`);
  } finally {
    clean(tmp);
  }
}

// -- --log option (Loader) ------------------------------------------------------

console.log("--log option...");
{
  const tmp = mkdtemp("goccia-log-");
  try {
    const source = [
      'console.log("log");',
      'console.warn("warn");',
      'console.error("error");',
      'console.info("info");',
      'console.debug("debug");',
      "console.dir({ value: 1 });",
      'console.assert(false, "assert");',
      'console.count("count");',
      'console.group("group");',
      'console.trace("trace");',
      "console.table([1, 2]);",
      "console.groupEnd();",
      'console.timeEnd("missing");',
      "",
    ].join("\n");
    const methods = [
      "log",
      "warn",
      "error",
      "info",
      "debug",
      "dir",
      "assert",
      "count",
      "group",
      "trace",
      "table",
      "timeEnd",
    ];

    const logPath = join(tmp, "run.log");
    const { exitCode, json, stderr } = runLoaderJson(source, [`--log=${logPath}`]);
    if (exitCode !== 0)
      throw new Error(`Log file run failed: ${JSON.stringify(json.error)}${stderr}`);
    if (!existsSync(logPath)) throw new Error(`Log file should exist at ${logPath}`);
    const content = readFileSync(logPath, "utf-8");
    for (const method of methods) {
      if (!content.includes(`[${method}]`))
        throw new Error(`Log file should contain [${method}], got: ${content}`);
    }
  } finally {
    clean(tmp);
  }
}

// -- Assertion failure text (TestRunner) ---------------------------------------

// A failed assertion is recorded rather than thrown, so its message cannot be
// observed from inside a test. These properties are load-bearing enough to pin
// from the outside: toBeInstanceOf naming what it compared, a rejected returned
// Promise naming the error it rejected with, and the vitest shim keeping a
// named, actionable error for every member it does not provide.
console.log("Assertion failure text...");
{
  const tmp = mkdtemp("goccia-assertion-text-");
  try {
    const instanceOfSrc = join(tmp, "instance-of.test.js");
    writeFileSync(
      instanceOfSrc,
      [
        "class BatchError extends Error {}",
        "class NamedError extends Error {}",
        "class Plain {}",
        "class Sibling {}",
        'describe("rendering", () => {',
        '  test("error subject", () => {',
        '    expect(new BatchError("boom")).toBeInstanceOf(NamedError);',
        "  });",
        '  test("plain subject", () => {',
        "    expect(new Plain()).toBeInstanceOf(Sibling);",
        "  });",
        "});",
        "",
      ].join("\n"),
    );
    const instanceOf = await $`${TESTRUNNER} ${instanceOfSrc} --no-progress 2>&1`.nothrow();
    const instanceOfOut = instanceOf.text();
    // The class object serializes as "{}", so a message built from the values
    // alone said "Expected {} to be an instance of {}" and named neither side.
    for (const expected of [
      "Expected Error: boom to be an instance of NamedError",
      "Expected Plain{} to be an instance of Sibling",
    ]) {
      if (!instanceOfOut.includes(expected))
        throw new Error(`toBeInstanceOf should report ${expected}, got: ${instanceOfOut}`);
    }

    // The reason a returned Promise rejected with is the whole failure report,
    // and an Error keeps "name" on its prototype and "message" non-enumerable,
    // so serializing the value reported `new Error("boom")` as "{}" — the one
    // shape a debugging session most needs named. A class extending Error
    // inherits Error.prototype.name, so its identity is read off the
    // constructor, while an explicitly assigned name still wins.
    const rejectionSrc = join(tmp, "rejection.test.js");
    writeFileSync(
      rejectionSrc,
      [
        "class MyErr extends Error {}",
        "class NamedErr extends Error {",
        "  constructor(message) { super(message); this.name = 'ValidationFailure'; }",
        "}",
        "class ProtoNamed extends Error {}",
        "ProtoNamed.prototype.name = 'ProtoAssigned';",
        // An explicit prototype name that happens to spell "Error" is still
        // the author's answer, so the constructor name must not displace it.
        "class ProtoErrorNamed extends Error {}",
        "ProtoErrorNamed.prototype.name = 'Error';",
        'test("plain error", () => Promise.reject(new Error("boom")));',
        'test("subclass error", () => Promise.reject(new MyErr("boom")));',
        'test("named subclass error", () => Promise.reject(new NamedErr("boom")));',
        'test("prototype-named subclass error", () => Promise.reject(new ProtoNamed("boom")));',
        'test("prototype-named Error subclass", () => Promise.reject(new ProtoErrorNamed("boom")));',
        'test("native error", () => Promise.reject(new TypeError("bad")));',
        'test("plain object", () => Promise.reject({ code: 42 }));',
        'test("message only", () => Promise.reject({ message: "hi" }));',
        "",
      ].join("\n"),
    );
    const rejection = await $`${TESTRUNNER} ${rejectionSrc} --no-progress 2>&1`.nothrow();
    const rejectionOut = rejection.text();
    for (const expected of [
      "Returned Promise rejected: Error: boom",
      "Returned Promise rejected: MyErr: boom",
      "Returned Promise rejected: ValidationFailure: boom",
      "Returned Promise rejected: ProtoAssigned: boom",
      "Returned Promise rejected: TypeError: bad",
      "Returned Promise rejected: { code: 42 }",
      "Returned Promise rejected: { message: 'hi' }",
    ]) {
      if (!rejectionOut.includes(expected))
        throw new Error(
          `TestRunner should report "${expected}", got: ${rejectionOut}`,
        );
    }
    // ProtoErrorNamed spells its prototype name "Error" on purpose, which
    // reads identically to the inherited default; only the absence of the
    // constructor name tells the two apart.
    if (rejectionOut.includes("ProtoErrorNamed: boom"))
      throw new Error(
        `TestRunner must keep an explicitly assigned "Error" prototype name, got: ${rejectionOut}`,
      );

    // Every member the shim does not implement must keep throwing by name. The
    // contract is what tells a suite author which member to work around, so
    // silently degrading one to a no-op is worse than not having it.
    const unsupported: Array<[string, string]> = [
      ["vi.hoisted(() => ({}))", "vi.hoisted is not supported"],
      ["vi.importMock('./x.js')", "vi.importMock is not supported"],
      ["vi.setConfig({})", "vi.setConfig is not supported"],
      // The fake-timer family is implemented, but three of its members are not
      // and must keep saying so by name rather than becoming absent properties:
      // no requestAnimationFrame, no process.nextTick, and no real elapsed time
      // for an auto-advancing clock to track. `setTimerTickMode("manual")` is
      // accepted — it names the only behaviour there is — so the unsupported
      // case has to ask for one of the others.
      [
        "vi.advanceTimersToNextFrame()",
        "vi.advanceTimersToNextFrame is not supported",
      ],
      ["vi.runAllTicks()", "vi.runAllTicks is not supported"],
      [
        "vi.setTimerTickMode('interval')",
        "vi.setTimerTickMode is not supported",
      ],
      ["vi.importActual('./x.js')", "vi.importActual is not supported"],
      ["vi.resetModules()", "vi.resetModules is not supported"],
      ["vi.doMock('./x.js')", "vi.doMock is not supported"],
    ];
    const shimSrc = join(tmp, "shim.test.js");
    writeFileSync(
      shimSrc,
      [
        'import { vi } from "vitest";',
        'describe("unsupported", () => {',
        ...unsupported.map(
          ([call], index) =>
            `  test("case ${index}", () => { try { ${call}; console.log("NO-THROW ${index}"); } catch (e) { console.log("THREW ${index}: " + e.message); } });`,
        ),
        "});",
        "",
      ].join("\n"),
    );
    const shim = await $`${TESTRUNNER} ${shimSrc} --source-type=module --no-progress 2>&1`.nothrow();
    const shimOut = shim.text();
    unsupported.forEach(([call, message], index) => {
      if (shimOut.includes(`NO-THROW ${index}`))
        throw new Error(`${call} must keep throwing its named error, got: ${shimOut}`);
      // The whole capture would pass the docs-link check as soon as any one
      // member carried the link, so each case is matched on its own line.
      const line = shimOut
        .split("\n")
        .find((entry) => entry.includes(`THREW ${index}: `));
      if (line === undefined || !line.includes(`THREW ${index}: ${message}`))
        throw new Error(`${call} should report "${message}", got: ${shimOut}`);
      if (!line.includes("docs/testing-api.md"))
        throw new Error(`${call} should point at the docs, got: ${line}`);
    });
  } finally {
    clean(tmp);
  }
}

// -- Runtime diagnostics (TestRunner) ------------------------------------------

// An uncaught runtime fault is reported by the runner, not by the suite, so the
// rendered diagnostic is only observable from outside: the named callee, a
// suggestion, a `--> file:line:column` header and a code frame, not a bare
// "Fatal error: TypeError: undefined is not a function".
console.log("Runtime diagnostics...");
{
  const tmp = mkdtemp("goccia-diagnostic-");
  try {
    const calleeSrc = join(tmp, "callee.test.js");
    writeFileSync(calleeSrc, ["const obj = {};", "obj.missingMethod();", ""].join("\n"));

    {
      const run = await $`${TESTRUNNER} ${calleeSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      // A file whose top level throws is a failed file: the runner exits 1.
      if (run.exitCode !== 1)
        throw new Error(
          `TestRunner should exit 1 on an uncaught throw, got ${run.exitCode}: ${out}`,
        );
      for (const expected of [
        "TypeError: obj.missingMethod is not a function",
        "Suggestion: 'obj' is of type 'object' which does not have method 'missingMethod'",
        // The call is located at the method's name, where Node puts it.
        "callee.test.js:2:5",
        "2 | obj.missingMethod();",
      ]) {
        if (!out.includes(expected))
          throw new Error(
            `TestRunner should report "${expected}", got: ${out}`,
          );
      }
      if (out.includes("Fatal error"))
        throw new Error(
          `TestRunner must render a thrown value as a diagnostic, not a fatal error, got: ${out}`,
        );
    }

    // The code frame is read from a file, and the file it was read from used to
    // be the entry every time: a module that threw while evaluating produced a
    // header naming the module and an excerpt quoting whatever the entry file
    // happened to have at that line number.
    const dep = join(tmp, "dep.js");
    writeFileSync(
      dep,
      [
        "// dep filler 1",
        "// dep filler 2",
        "// dep filler 3",
        "// dep filler 4",
        "export const boom = (() => { throw new Error('dep exploded'); })();",
        "",
      ].join("\n"),
    );
    const entry = join(tmp, "entry.test.js");
    writeFileSync(
      entry,
      [
        "// entry filler 1",
        "// entry filler 2",
        "// entry filler 3",
        "// entry filler 4",
        "import { boom } from './dep.js';",
        "console.log(boom);",
        "",
      ].join("\n"),
    );

    {
      const run = await $`${TESTRUNNER} ${entry} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (run.exitCode !== 1)
        throw new Error(
          `TestRunner should exit 1 on a module that throws, got ${run.exitCode}: ${out}`,
        );
      if (!out.includes("Error: dep exploded"))
        throw new Error(`TestRunner should report the module's error, got: ${out}`);
      if (!out.includes("dep.js:5:"))
        throw new Error(`TestRunner should locate the fault in dep.js, got: ${out}`);
      if (!out.includes("throw new Error('dep exploded')"))
        throw new Error(
          `TestRunner should quote dep.js in the code frame, got: ${out}`,
        );
      if (out.includes("entry filler") || out.includes("import { boom }"))
        throw new Error(
          `TestRunner must not quote the entry file for a fault in dep.js, got: ${out}`,
        );
    }

    // A `new` before an unrelated throw must not drag its location onto the
    // throw: the constructor-site stamp is scoped to the construction, so
    // `m.missing.x` reports its own line.
    const precedingNewSrc = join(tmp, "preceding-new.test.js");
    writeFileSync(
      precedingNewSrc,
      [
        "const make = () => {",
        "  const m = new Map();",
        "  return m.missing.x;",
        "};",
        "make();",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${precedingNewSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (!out.includes("preceding-new.test.js:3:"))
        throw new Error(
          `TestRunner should locate the fault at the m.missing.x line (3), not the new Map() line, got: ${out}`,
        );
      if (out.includes("preceding-new.test.js:2:"))
        throw new Error(
          `TestRunner leaked the new Map() location (line 2) onto a later throw, got: ${out}`,
        );
    }

    // A successful construct must not leave its constructor's position stamped
    // on the caller frame: a later native error reports its own line, not the
    // `new`'s. `new Map()` on line 1, `JSON.parse("{")` on line 2 — the syntax
    // error must locate to line 2.
    const constructSrc = join(tmp, "construct-stamp.test.js");
    writeFileSync(
      constructSrc,
      ['new Map();', 'JSON.parse("{");', ""].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${constructSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (!out.includes("construct-stamp.test.js:2:"))
        throw new Error(
          `TestRunner should locate the JSON error at line 2 (JSON.parse), got: ${out}`,
        );
      if (out.includes("construct-stamp.test.js:1:"))
        throw new Error(
          `TestRunner leaked the new Map() position (line 1) onto the JSON error: ${out}`,
        );
    }

    // The automatic SuppressedError from a block+disposer double-throw carries
    // genuine provenance (built as an error object, not a bare object): it
    // renders as a "SuppressedError" diagnostic with a location.
    const suppSrc = join(tmp, "suppressed.test.js");
    writeFileSync(
      suppSrc,
      [
        "const run = () => {",
        '  { using d = { [Symbol.dispose]() { throw new Error("disposer boom"); } }; throw new Error("body boom"); }',
        "};",
        "run();",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${suppSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (!out.includes("SuppressedError"))
        throw new Error(`TestRunner should report a SuppressedError, got: ${out}`);
      if (!out.includes("suppressed.test.js:"))
        throw new Error(
          `TestRunner should locate the SuppressedError, not print it bare, got: ${out}`,
        );
    }

    // A non-constructable super() reaches the class-construction machinery, not
    // the ordinary call guard, so its message/location parity is governed there
    // (a separate concern from these diagnostics). Pin only what this change
    // owns: it renders as a TypeError diagnostic rather than a bare
    // "Fatal error". Byte-level super() parity is tracked with the construction
    // path, not here.
    const superSrc = join(tmp, "super-call.test.js");
    writeFileSync(
      superSrc,
      [
        "class B extends null {",
        "  constructor() {",
        "    super();",
        "  }",
        "}",
        "new B();",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${superSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (out.includes("Fatal error"))
        throw new Error(
          `TestRunner must render the super() fault as a diagnostic, not a fatal error, got: ${out}`,
        );
      if (!out.includes("TypeError"))
        throw new Error(
          `TestRunner should report a TypeError for a non-constructable super(), got: ${out}`,
        );
    }

    // Security: a code frame is rendered only from provenance the engine
    // recorded on a genuine error at creation, never from the thrown value's
    // guest-writable `stack` string. A forged `{ stack: "...at f (FILE:1:1)" }`
    // object carries no provenance, so no host opens FILE and no frame is shown.
    const secretPath = join(tmp, "SECRET_MUST_NOT_APPEAR.txt");
    const secretMarker = "TOP_SECRET_FILE_CONTENTS_LEAKED";
    writeFileSync(secretPath, `${secretMarker}\nline two of the secret file\n`);
    const attackSrc = join(tmp, "attack.test.js");
    // Point the forged frame at the secret file. If a host reads it, the marker
    // (its line 1) shows up in the rendered code frame.
    writeFileSync(
      attackSrc,
      [
        `const forged = { name: "Error", message: "forged", stack: "Error: forged\\n    at f (${secretPath.replace(/\\/g, "\\\\")}:1:1)" };`,
        "throw forged;",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${attackSrc} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(secretMarker))
        throw new Error(
          `SECURITY: TestRunner read a file named by a thrown stack: ${out}`,
        );
      // Same attack through GocciaRunner and the bare loader. Sandbox mode, the
      // path that would otherwise bypass the sandbox.fs.path gate, is covered in
      // test-cli-apps.ts.
      for (const bin of [RUNNER, BARE]) {
        const run = await $`${bin} ${attackSrc} 2>&1`.nothrow();
        const out = run.text();
        if (out.includes(secretMarker))
          throw new Error(
            `SECURITY: ${bin} read a file named by a thrown stack: ${out}`,
          );
      }
    }

    // A frame naming a FIFO must not make any host block on open(); the registry
    // lookup misses and no filesystem call is made. Guard with a hard timeout so
    // a regression hangs the suite visibly rather than silently.
    if (process.platform !== "win32") {
      const fifoPath = join(tmp, "frame.fifo");
      const mk = await $`mkfifo ${fifoPath}`.nothrow();
      if (mk.exitCode === 0) {
        const fifoSrc = join(tmp, "fifo.test.js");
        writeFileSync(
          fifoSrc,
          [
            `const forged = { name: "Error", message: "f", stack: "Error: f\\n    at g (${fifoPath.replace(/\\/g, "\\\\")}:1:1)" };`,
            "throw forged;",
            "",
          ].join("\n"),
        );
        const proc = Bun.spawnSync(
          [TESTRUNNER, fifoSrc, "--no-progress"],

          { stdout: "pipe", stderr: "pipe", timeout: 20_000 },
        );
        // spawnSync kills a run that exceeds the timeout with a signal; a clean
        // exit means the diagnostic rendered without ever opening the FIFO.
        if (proc.signalCode)
          throw new Error(
            `SECURITY: TestRunner hung rendering a frame that names a FIFO (${proc.signalCode})`,
          );
      }
    }

    // Host-preload disclosure: a module the HOST preloaded (which the guest
    // never imported) must not be readable through a forged frame. The guest
    // never throws from it, so it is never a genuine error's provenance; the
    // forged object has none. The preloaded module's private source must not
    // appear.
    const preloadMarker = "HOST_PRELOAD_PRIVATE_SOURCE_MARKER";
    const preloadSrc = join(tmp, "preload.js");
    writeFileSync(
      preloadSrc,
      [
        `// ${preloadMarker}`,
        "export const hostValue = 42;",
        "",
      ].join("\n"),
    );
    const preloadAttack = join(tmp, "preload-attack.js");
    writeFileSync(
      preloadAttack,
      [
        `const forged = { name: "Error", message: "p", stack: "Error: p\\n    at f (${preloadSrc.replace(/\\/g, "\\\\")}:1:1)" };`,
        "throw forged;",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${RUNNER} ${preloadAttack} --globals ${preloadSrc} 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(preloadMarker))
        throw new Error(
          `SECURITY: Loader disclosed a host-preloaded module via a forged frame: ${out}`,
        );
    }

    // Cross-run stale disclosure: in one TestRunner invocation over two files,
    // file A loads a module carrying a secret; file B forges a frame naming that
    // module. Each file runs in its own engine scope, and B's forged object has
    // no provenance, so B must not surface A's module source.
    const staleMarker = "PRIOR_RUN_MODULE_SOURCE_MARKER";
    const staleDir = join(tmp, "stale");
    mkdirSync(staleDir, { recursive: true });
    writeFileSync(
      join(staleDir, "secret-mod.js"),
      [`// ${staleMarker}`, "export const v = 1;", ""].join("\n"),
    );
    writeFileSync(
      join(staleDir, "a.test.js"),
      ['import { v } from "./secret-mod.js";', "void v;", ""].join("\n"),
    );
    const secretModPath = join(staleDir, "secret-mod.js");
    writeFileSync(
      join(staleDir, "b.test.js"),
      [
        `const forged = { name: "Error", message: "b", stack: "Error: b\\n    at f (${secretModPath.replace(/\\/g, "\\\\")}:1:1)" };`,
        "throw forged;",
        "",
      ].join("\n"),
    );
    {
      const run = await $`${TESTRUNNER} ${join(staleDir, "a.test.js")} ${join(staleDir, "b.test.js")} --no-progress 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(staleMarker))
        throw new Error(
          `SECURITY: TestRunner disclosed a prior run's module source via a forged frame: ${out}`,
        );
    }

    // Host-preload GENUINE-error exclusion: a real error thrown INSIDE a
    // host-preloaded module (not forged) must still withhold that module's
    // source from the guest — location line yes, source lines no. The guest
    // calls a host global whose body throws; the host module's own source lines
    // must not appear.
    const hostBodyMarker = "HOST_MODULE_BODY_SOURCE_LINE";
    const hostThrower = join(tmp, "host-thrower.js");
    writeFileSync(
      hostThrower,
      [
        `// ${hostBodyMarker}`,
        "export const boom = () => {",
        `  const secret = "${hostBodyMarker}";`,
        "  return secret.length.toFixed.call(null).nope();",
        "};",
        "",
      ].join("\n"),
    );
    const hostThrowerMain = join(tmp, "host-thrower-main.js");
    writeFileSync(hostThrowerMain, ["boom();", ""].join("\n"));
    {
      const run = await $`${RUNNER} ${hostThrowerMain} --globals ${hostThrower} --compat-asi 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(hostBodyMarker))
        throw new Error(
          `SECURITY: Loader disclosed a host module's source for an error thrown inside it: ${out}`,
        );
    }

    // Held-error exclusion: a host-preloaded factory creates an Error; the guest
    // throws it later. The excerpt (host module source) must not be shown.
    const heldMarker = "HOST_HELD_FACTORY_SOURCE";
    const heldHost = join(tmp, "held-host.js");
    writeFileSync(
      heldHost,
      [`// ${heldMarker}`, 'export const makeHeld = () => new Error("held");', ""].join("\n"),
    );
    const heldMain = join(tmp, "held-main.js");
    writeFileSync(heldMain, ["const e = makeHeld();", "throw e;", ""].join("\n"));
    {
      const run = await $`${RUNNER} ${heldMain} --globals ${heldHost} --compat-asi 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(heldMarker))
        throw new Error(
          `SECURITY: Loader disclosed a host factory's source via a held error: ${out}`,
        );
    }

    // Deferred import-chain ownership: the host-injected module has finished
    // loading before guest execution starts. Calling its exported function
    // later must still carry host ownership into import(), transitively; a
    // synchronous host-preload depth counter cannot protect this shape.
    const deferredMarker = "DEFERRED_HOST_IMPORT_PRIVATE_SOURCE_XZ";
    const deferredSecret = join(tmp, "deferred-host-secret.js");
    writeFileSync(
      deferredSecret,
      [
        `// ${deferredMarker}`,
        `throw new Error("deferred host import failed"); // ${deferredMarker}`,
        "export const never = 1;",
        "",
      ].join("\n"),
    );
    const deferredGlobals = join(tmp, "deferred-globals.js");
    writeFileSync(
      deferredGlobals,
      [
        'export const loadDeferredHostModule = () => import("./deferred-host-secret.js");',
        "",
      ].join("\n"),
    );
    const deferredMain = join(tmp, "deferred-main.js");
    writeFileSync(deferredMain, ["await loadDeferredHostModule();", ""].join("\n"));
    {
      const run = await $`${RUNNER} ${deferredMain} --globals ${deferredGlobals} --compat-asi --source-type=module 2>&1`.nothrow();
      const out = run.text();
      if (run.exitCode === 0 || !out.includes("deferred-host-secret.js:2:"))
        throw new Error(
          `Loader should locate the deferred host import failure, got: ${out}`,
        );
      if (out.includes(deferredMarker))
        throw new Error(
          `SECURITY: Loader disclosed a deferred host import's source: ${out}`,
        );
    }

    // Memory bound: a guest module of long lines whose factory throws a genuine
    // TypeError, held many times, must not retain diagnostic excerpts past the
    // --max-memory ceiling. At a tight budget the run must hit the ceiling
    // (RangeError or the uncatchable "exceed the memory budget"), never
    // HELD:500 (unbounded retention) and never a nil-deref crash.
    const memDir = join(tmp, "mem");
    mkdirSync(memDir, { recursive: true });
    const longLine = "// " + "é😀".repeat(700);
    const memLines: string[] = [];
    for (let i = 0; i < 9; i++) memLines.push(longLine);
    memLines.push("export const boom = () => { const z = null; return z.x; };");
    for (let i = 0; i < 9; i++) memLines.push("// " + "B".repeat(2000));
    writeFileSync(join(memDir, "mod.js"), memLines.join("\n") + "\n");
    writeFileSync(
      join(memDir, "main.js"),
      [
        'import { boom } from "./mod.js";',
        "const held = [];",
        "Array.from({ length: 500 }).forEach(() => { try { boom(); } catch (e) { held.push(e); } });",
        'console.log("HELD:" + held.length);',
        "",
      ].join("\n"),
    );
    {
      const run = await $`${RUNNER} ${join(memDir, "main.js")} --max-memory=262144 --compat-asi --source-type=module 2>&1`.nothrow();
      const out = run.text();
      if (out.includes("Object reference is Nil") || out.includes("Range check"))
        throw new Error(
          `Loader crashed under a diagnostic memory bound: ${out}`,
        );
      if (out.includes("HELD:500"))
        throw new Error(
          `Loader retained 500 diagnostic excerpts past --max-memory (unbounded): ${out}`,
        );
      if (!out.includes("RangeError") && !out.includes("exceed the memory budget"))
        throw new Error(
          `Loader should hit the memory ceiling, got: ${out}`,
        );
    }

    // Virtual-module ownership: a module the HOST injected (`--module`) is
    // host-owned even though the GUEST imports it and a genuine error is thrown
    // from inside it. The guest must see the fault's location but never the
    // module's source text. (The guest has no API to inject a virtual module, so
    // every virtual module is host-provided; ownership is decided at load, not
    // inferred from who is running when the frame is captured.)
    const vmMarker = "VIRTUAL_MODULE_BODY_SOURCE_XZ";
    const vmMain = join(tmp, "vmodule-main.js");
    writeFileSync(vmMain, ['import { boom } from "virtual:secret";', "boom();", ""].join("\n"));
    const vmDef =
      `virtual:secret=// ${vmMarker}\n` +
      "export const boom = () => { const z = null; return z.x; };";
    {
      const run = await $`${RUNNER} ${vmMain} --module ${vmDef} --compat-asi --source-type=module 2>&1`.nothrow();
      const out = run.text();
      if (out.includes(vmMarker))
        throw new Error(
          `SECURITY: Loader disclosed a host --module's source to a guest: ${out}`,
        );
      if (!out.includes("virtual:secret:"))
        throw new Error(
          `Loader should still locate the fault in the virtual module, got: ${out}`,
        );
    }

    // Canonical-identity ownership: a second spelling of a host-preloaded file
    // must resolve to its first, host-owned entry. POSIX uses device+inode;
    // Windows uses volume serial + file index from the already-open handle.
    const aliasMarker = "HOST_FILE_VIA_ALIAS_SOURCE_XZ";
    const aliasChildMarker = "HOST_ALIAS_TRANSITIVE_IMPORT_SOURCE_XZ";
    const hostDirectory = join(tmp, "canonical-host");
    mkdirSync(hostDirectory, { recursive: true });
    const aliasChildSource = [
      `// ${aliasChildMarker}`,
      `throw new Error("alias child failed"); // ${aliasChildMarker}`,
      "export const never = 1;",
      "",
    ].join("\n");
    writeFileSync(join(hostDirectory, "alias-child.js"), aliasChildSource);
    // A symlink/hardlink module resolves a relative child beside the alias;
    // a directory junction resolves it inside hostDirectory. Populate both
    // locations with the same source so each platform exercises its real rule.
    writeFileSync(join(tmp, "alias-child.js"), aliasChildSource);
    const realHost = join(hostDirectory, "real-host.js");
    writeFileSync(
      realHost,
      [
        `// ${aliasMarker}`,
        "export const boom = () => { const z = null; return z.x; };",
        'export const loadAliasChild = () => import("./alias-child.js");',
        "",
      ].join("\n"),
    );
    const aliases: Array<{ path: string; label: string }> = [];
    if (process.platform === "win32") {
      const hardlink = join(tmp, "hardlink-host.js");
      try {
        linkSync(realHost, hardlink);
        aliases.push({ path: hardlink, label: "hardlink" });
      } catch {
        // The filesystem may not support hardlinks in the CI workspace.
      }
      const junctionDirectory = join(tmp, "junction-host");
      try {
        symlinkSync(hostDirectory, junctionDirectory, "junction");
        aliases.push({
          path: join(junctionDirectory, "real-host.js"),
          label: "junction",
        });
      } catch {
        // Junction creation can be disabled by the runner's filesystem policy.
      }
    } else {
      const symlink = join(tmp, "symlink-host.js");
      try {
        symlinkSync(realHost, symlink);
        aliases.push({ path: symlink, label: "symlink" });
      } catch {
        // Some filesystems disallow symlinks; skip rather than false-fail.
      }
      // A hard link is the host file under a second name that no canonical
      // path resolves, on POSIX as on Windows.
      const hardlink = join(tmp, "hardlink-host.js");
      try {
        linkSync(realHost, hardlink);
        aliases.push({ path: hardlink, label: "hardlink" });
      } catch {
        // The filesystem may not support hardlinks.
      }
    }
    for (const alias of aliases) {
      if (!existsSync(alias.path)) continue;
      const aliasMain = join(tmp, `${alias.label}-main.js`);
      const relativeAlias = `./${alias.path
        .slice(tmp.length + 1)
        .replace(/\\/g, "/")}`;
      writeFileSync(
        aliasMain,
        [`import { boom } from "${relativeAlias}";`, "boom();", ""].join("\n"),
      );
      {
        const run = await $`${RUNNER} ${aliasMain} --globals ${realHost} --compat-asi --source-type=module 2>&1`.nothrow();
        const out = run.text();
        if (out.includes(aliasMarker))
          throw new Error(
            `SECURITY: Loader disclosed a host file's source via a Windows/POSIX ${alias.label}: ${out}`,
          );
      }

      const transitiveMain = join(tmp, `${alias.label}-transitive-main.js`);
      writeFileSync(
        transitiveMain,
        [
          `import { loadAliasChild } from "${relativeAlias}";`,
          "await loadAliasChild();",
          "",
        ].join("\n"),
      );
      {
        const run = await $`${RUNNER} ${transitiveMain} --globals ${realHost} --compat-asi --source-type=module 2>&1`.nothrow();
        const out = run.text();
        if (run.exitCode === 0 || !out.includes("alias-child.js:2:"))
          throw new Error(
            `Loader should locate the ${alias.label} host child failure, got: ${out}`,
          );
        if (out.includes(aliasChildMarker))
          throw new Error(
            `SECURITY: Loader disclosed source imported through a host ${alias.label}: ${out}`,
          );
      }
    }
  } finally {
    clean(tmp);
  }
}

// -- Entry module linking ------------------------------------------------------

// ES2026 §16.2.1.6.1.2 Link(): a name the entry imports or re-exports that does
// not resolve is a SyntaxError before any module of the graph evaluates. The
// entry is the file the runner was given, so a suite cannot observe its own
// link failure; these run the CLI instead (#1274).
// Everything a script printed before the runner's timing lines.
// Line endings are normalized, because Windows writes CRLF.
const scriptStdout = (out: string): string => {
  const text = out.replace(/\r\n/g, "\n");
  const at = text.indexOf("Running script (");
  return at < 0 ? text : text.slice(0, at);
};

console.log("Undeclared name: Suggestion line...");
{
  // A read of an undeclared name throws the intrinsic ReferenceError with
  // the declare-before-use suggestion (#1494 item 10).
  const tmp = mkdtemp("goccia-undeclared-suggestion-");
  try {
    const sources: Record<string, string> = {
      "read.js": "missingName;\n",
      "assign.js": "let total = 0;\nmissingTarget = total;\n",
      "compound.js": "let total = 0;\ntotal += 1;\nmissingCompound += 1;\n",
    };
    for (const [name, source] of Object.entries(sources)) {
      const path = join(tmp, name);
      writeFileSync(path, source);
      const run = await $`${RUNNER} ${path} 2>&1`.nothrow().quiet();
      const out = run.text();
      if (run.exitCode !== 1)
        throw new Error(`${name} should exit 1, got ${run.exitCode}: ${out}`);
      if (!/ReferenceError: missing\w+ is not defined/.test(out))
        throw new Error(`${name} should report a ReferenceError, got: ${out}`);
      if (!out.includes("Suggestion: check the spelling or declare the variable with const, let, or var before use"))
        throw new Error(`${name} should print the declare-before-use suggestion, got: ${out}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("console.log renders functions, classes and errors as util.inspect does...");
{
  // Functions, classes and errors printed as `{}` (#1494 item 9). The
  // expected lines are Node's util.inspect output for the same script.
  const tmp = mkdtemp("goccia-console-inspect-");
  try {
    const fns = join(tmp, "fns.js");
    writeFileSync(
      fns,
      [
        "const named = () => 1;",
        "const obj = { method() {} };",
        "class Base {}",
        "class Derived extends Base {}",
        "class WithStatic { static count = 2; }",
        "const tagged = () => 1;",
        'tagged.label = "t";',
        "console.log(() => 1);",
        "console.log(named);",
        "console.log(obj.method);",
        "console.log(Base);",
        "console.log(Derived);",
        "console.log(class {});",
        "console.log(WithStatic);",
        "console.log(tagged);",
        "console.log(async () => {});",
        "console.log(Math.max);",
        "console.log([named, Base]);",
        "console.log({ f: named });",
        "",
      ].join("\n"),
    );
    const expectedFns = [
      "[Function (anonymous)]",
      "[Function: named]",
      "[Function: method]",
      "[class Base]",
      "[class Derived extends Base]",
      "[class (anonymous)]",
      "[class WithStatic] { count: 2 }",
      "[Function: tagged] { label: 't' }",
      "[AsyncFunction (anonymous)]",
      "[Function: max]",
      "[ [Function: named], [class Base] ]",
      "{ f: [Function: named] }",
      "",
    ].join("\n");
    const run = await $`${RUNNER} ${fns}`.nothrow().quiet();
    const out = scriptStdout(run.stdout.toString());
    if (run.exitCode !== 0 || out !== expectedFns)
      throw new Error(`Function and class rendering should be:\n${expectedFns}\ngot (exit ${run.exitCode}):\n${out}${run.stderr.toString()}`);

    // --inspect-depth sets how many levels deep console output renders (5
    // by default); 0 renders one level, as any value below 1 does.
    const deep = join(tmp, "inspect-depth.js");
    writeFileSync(deep, "console.log({ a: { b: { c: { d: { e: { f: { g: 1 } } } } } } });\n");
    for (const [flags, expected] of [
      [[], "{ a: { b: { c: { d: { e: [Object] } } } } }"],
      [["--inspect-depth=2"], "{ a: { b: [Object] } }"],
      [["--inspect-depth=0"], "{ a: [Object] }"],
      [["--inspect-depth=10"], "{ a: { b: { c: { d: { e: { f: { g: 1 } } } } } } }"],
    ] as const) {
      const depthRun = await $`${RUNNER} ${flags} ${deep}`.nothrow().quiet();
      const depthOut = scriptStdout(depthRun.stdout.toString()).trim();
      if (depthRun.exitCode !== 0 || depthOut !== expected)
        throw new Error(`${flags.join(" ") || "no flag"} should render ${expected}, got (exit ${depthRun.exitCode}): ${depthOut}${depthRun.stderr.toString()}`);
    }

    const errors = join(tmp, "errors.js");
    writeFileSync(
      errors,
      [
        'const error = new TypeError("boom");',
        "error.code = 5;",
        "console.log(error);",
        "console.log({ error });",
        'console.log(new Error("outer", { cause: new RangeError("inner") }));',
        "",
      ].join("\n"),
    );
    // An error prints its stack, then its own properties; nested, its stack
    // lines are indented under the entry, and `cause` reads `[cause]`.
    const expectedErrors = [
      "TypeError: boom",
      "    at <module> (errors.js:1:15) {",
      "  code: 5",
      "}",
      "{",
      "  error: TypeError: boom",
      "      at <module> (errors.js:1:15) {",
      "    code: 5",
      "  }",
      "}",
      "Error: outer",
      "    at <module> (errors.js:5:13) {",
      "  [cause]: RangeError: inner",
      "      at <module> (errors.js:5:41)",
      "}",
      "",
    ].join("\n");
    const errorsRun = await $`${resolve(RUNNER)} errors.js`.cwd(tmp).nothrow().quiet();
    const errorsOut = scriptStdout(errorsRun.stdout.toString());
    if (errorsRun.exitCode !== 0 || errorsOut !== expectedErrors)
      throw new Error(`Error rendering should be:\n${expectedErrors}\ngot (exit ${errorsRun.exitCode}):\n${errorsOut}${errorsRun.stderr.toString()}`);

    // Inside console.group every line of a multi-line value is indented.
    const grouped = join(tmp, "grouped.js");
    writeFileSync(
      grouped,
      'console.group("G");\nconsole.log(new Error("in group"));\nconsole.groupEnd();\n',
    );
    const groupRun = await $`${resolve(RUNNER)} grouped.js`.cwd(tmp).nothrow().quiet();
    const expectedGroup = "G\n  Error: in group\n      at <module> (grouped.js:2:13)\n";
    if (scriptStdout(groupRun.stdout.toString()) !== expectedGroup)
      throw new Error(`Grouped error should be:\n${expectedGroup}\ngot:\n${groupRun.stdout.toString()}`);
  } finally {
    clean(tmp);
  }
}

console.log("--max-memory stack refusal is located at the call...");
{
  // A refused stack growth is settled at the first instruction boundary in
  // the callee, since a call skips the boundary; the RangeError is located at
  // the call that needed the stack, as an ordinary stack overflow is (#1494
  // item 11), at the callee's name (ADR 0135), whether or not the callee's
  // first instruction has a source position of its own.
  const tmp = mkdtemp("goccia-stack-memory-location-");
  try {
    const cases: { name: string; body: string[]; expected: string[] }[] = [
      {
        // The callee's first instruction has no source position.
        name: "block.js",
        body: [
          "const r = (n) => {",
          "  const o = { n };",
          "  return n === 0 ? 0 : r(n - 1) + 1;",
          "};",
        ],
        expected: ["--> block.js:5:24", "5 |   return n === 0 ? 0 : r(n - 1) + 1;"],
      },
      {
        // The callee's first instruction is the located `n === 0`.
        name: "expression.js",
        body: [
          "const r = (n) =>",
          "  n === 0 ? 0 : r(n - 1) + 1;",
        ],
        expected: ["--> expression.js:4:17", "4 |   n === 0 ? 0 : r(n - 1) + 1;"],
      },
    ];
    for (const { name, body, expected: located } of cases) {
      writeFileSync(
        join(tmp, name),
        [
          "const keep = [];",
          // Leave headroom for per-object size changes: the preload must fit
          // inside 8 MiB so stack growth, not array construction, is refused.
          "for (let i = 0; i < 20000; i = i + 1) { keep.push({ i, a: [i, i, i, i] }); }",
          ...body,
          "console.log(r(1000000));",
          "",
        ].join("\n"),
      );
      // The depth limit is raised past the recursion, so the memory limit is
      // what stops it on every platform: a 32-bit build's smaller objects
      // leave room for the default 2,200 frames inside 8 MiB.
      const run = await $`${resolve(RUNNER)} ${name} --compat-traditional-for-loop --max-memory=8MiB --max-stack=2000000 2>&1`.cwd(tmp).nothrow().quiet();
      const out = run.text();
      if (run.exitCode !== 1)
        throw new Error(`Stack memory refusal (${name}) should exit 1, got ${run.exitCode}: ${out}`);
      for (const expected of [
        "RangeError: Maximum call stack size exceeded",
        "Suggestion: the call stack reached the memory limit",
        ...located,
      ]) {
        if (!out.includes(expected))
          throw new Error(`Stack memory refusal (${name}) should report "${expected}", got: ${out}`);
      }
      if (out.includes(":0:0"))
        throw new Error(`Stack memory refusal (${name}) must not be located at 0:0, got: ${out}`);
    }
  } finally {
    clean(tmp);
  }
}

console.log("Entry module linking...");
{
  const tmp = mkdtemp("goccia-entry-link-");
  try {
    const files: Record<string, string> = {
      "marker.js": 'console.log("marker evaluated");\nexport const ready = "ready";\n',
      "dep.js": 'console.log("dep evaluated");\nexport const present = 1;\n',
      "ns-barrel.js": 'console.log("barrel evaluated");\nexport * as ns from "./dep.js";\n',
      "star-a.js": 'console.log("star-a evaluated");\nexport const dup = "a";\n',
      "star-b.js": 'console.log("star-b evaluated");\nexport const dup = "b";\n',
      "star-both.js": 'export * from "./star-a.js";\nexport * from "./star-b.js";\n',
      "cycle-a.js": 'import { nothere } from "./cycle-b.js";\nconsole.log("cycle-a evaluated");\nexport const fromA = 1;\n',
      "cycle-b.js": 'import { fromA } from "./cycle-a.js";\nconsole.log("cycle-b evaluated");\nexport const fromB = 1;\n',
    };
    for (const [name, source] of Object.entries(files)) writeFileSync(join(tmp, name), source);

    const cases: Array<[string, string, string]> = [
      ["named", 'import { missing } from "./dep.js";', 'Module "./dep.js" has no export named "missing"'],
      ["aliased", 'import { missing as renamed } from "./dep.js";', 'Module "./dep.js" has no export named "missing"'],
      ["default", 'import fallback from "./dep.js";', 'Module "./dep.js" has no export named "default"'],
      ["re-export", 'export { missing } from "./dep.js";', 'Module "./dep.js" has no export named "missing"'],
      ["namespace re-export", 'import { ns, nope } from "./ns-barrel.js";', 'Module "./ns-barrel.js" has no export named "nope"'],
      ["star-export ambiguity", 'import { dup } from "./star-both.js";', 'Module "./star-both.js" has no export named "dup"'],
      ["cycle", 'import "./cycle-a.js";', 'Module "./cycle-b.js" has no export named "nothere"'],
    ];
    for (const [label, badImport, message] of cases) {
      const entry = join(tmp, "entry.js");
      // A valid import ahead of the bad one: linking the whole graph first means
      // even it must not evaluate.
      writeFileSync(
        entry,
        ['import { ready } from "./marker.js";', badImport, 'console.log("entry evaluated");', ""].join("\n"),
      );
      for (const sourceType of ["script", "module"]) {
        const run = await $`${RUNNER} ${entry} --source-type=${sourceType} 2>&1`.nothrow().quiet();
        const out = run.text();
        if (run.exitCode !== 1)
          throw new Error(`Entry link (${label}, ${sourceType}) should exit 1, got ${run.exitCode}: ${out}`);
        if (!out.includes(`SyntaxError: ${message}`))
          throw new Error(`Entry link (${label}, ${sourceType}) should throw SyntaxError: ${message}, got: ${out}`);
        const location = label === "cycle" ? "cycle-a.js:1:1" : "entry.js:2:1";
        if (!out.includes(location))
          throw new Error(`Entry link (${label}, ${sourceType}) should report ${location}, got: ${out}`);
        // Diagnostic code frames can contain the source of the console calls.
        if (out.split(/\r?\n/).some((line) => /^[a-z-]+ evaluated$/.test(line.trim())))
          throw new Error(`Entry link (${label}, ${sourceType}) must fail before any module evaluates, got: ${out}`);
      }
    }

    // Each REPL input links its own requests: a failed input evaluates nothing,
    // and the module it linked evaluates once a later input imports it.
    const repl = Bun.spawnSync([join(process.cwd(), REPL)], {
      cwd: tmp,
      stdin: new TextEncoder().encode(
        [
          'import { ready } from "./marker.js"; import { missing } from "./dep.js";',
          'import { ready } from "./marker.js"; [ready, "linked"].join(":");',
          "",
        ].join("\n"),
      ),
      stdout: "pipe",
      stderr: "pipe",
    });
    const output = normalizeLineEndings(repl.stdout.toString() + repl.stderr.toString());
    if (!output.includes('SyntaxError: Module "./dep.js" has no export named "missing"'))
      throw new Error(`REPL should reject a missing import at link time, got: ${output}`);
    if (output.includes("dep evaluated") || output.split("marker evaluated").length !== 2)
      throw new Error(`REPL should evaluate only the later input's module, once, got: ${output}`);
    if (!output.includes("ready:linked"))
      throw new Error(`REPL should run the input after a failed link, got: ${output}`);
  } finally {
    clean(tmp);
  }
}

// -- Entry module evaluated once (#1447) --------------------------------------

// ES2026 §16.2.1.6.1.3.1 InnerModuleEvaluation: a module record evaluates once,
// and an import that resolves to the entry's own file must get the entry's own
// record (§16.2.1.10 HostLoadImportedModule). The entry is the file the runner
// was given, so the path it was named by (relative, ./, absolute) and the .gbc
// form can only be exercised from the CLI. tests/language/modules covers the
// test runner, live bindings and the temporal dead zone.
console.log("Entry module evaluated once...");
{
  const tmp = mkdtemp("goccia-entry-once-");
  // The runner names a relative entry by the working directory getcwd()
  // reports, and on macOS that has the /var -> /private/var symlink resolved
  // while tmpdir() keeps /var. abs.mjs must import itself by the spelling
  // the runner gives it; one file reached through two spellings is #1498.
  // Plain realpathSync keeps a Windows 8.3 name, as getcwd() does there.
  const runnerTmp = realpathSync(tmp);
  try {
    const files: Record<string, string> = {
      "self.mjs": 'import "./self.mjs";\nconsole.log("self ran");\n',
      "abs.mjs": `import ${JSON.stringify(join(runnerTmp, "abs.mjs"))};\nconsole.log("abs ran");\n`,
      "id.mjs": 'export const o = {};\nimport { o as p } from "./id.mjs";\nconsole.log("same record:", o === p);\n',
      "cycle-a.mjs": 'import "./cycle-b.mjs";\nconsole.log("a ran");\n',
      "cycle-b.mjs": 'import "./cycle-a.mjs";\nconsole.log("b ran");\n',
      "self.js": 'import "./self.js";\nconsole.log("self ran");\n',
      "id.js": 'export const o = {};\nimport { o as p } from "./id.js";\nconsole.log("same record:", o === p);\n',
      "cycle-a.js": 'import "./cycle-b.js";\nconsole.log("a ran");\n',
      "cycle-b.js": 'import "./cycle-a.js";\nconsole.log("b ran");\n',
      "bundled.mjs": 'import "./bundled.gbc";\nconsole.log("bundled ran");\n',
    };
    for (const [name, source] of Object.entries(files)) writeFileSync(join(tmp, name), source);

    const programOutput = (out: string): string[] =>
      normalizeLineEndings(out)
        .split("\n")
        .filter((line) => line.endsWith(" ran") || line.startsWith("same record:"));
    const expectRun = async (label: string, args: string[], expected: string[]): Promise<void> => {
      const run = await $`${join(process.cwd(), RUNNER)} ${args}`.cwd(tmp).nothrow().quiet();
      const out = run.text() + run.stderr.toString();
      const lines = programOutput(out);
      if (run.exitCode !== 0 || JSON.stringify(lines) !== JSON.stringify(expected))
        throw new Error(`${label}: expected ${JSON.stringify(expected)}, got exit ${run.exitCode}: ${out}`);
    };

    // Module source: the entry record is the one its own path resolves to.
    for (const entry of ["self.mjs", "./self.mjs", join(tmp, "self.mjs")])
      await expectRun(`module entry ${entry}`, [entry], ["self ran"]);
    await expectRun("module entry importing itself by absolute path",
      ["abs.mjs"], ["abs ran"]);
    await expectRun("module entry record identity",
      ["id.mjs"], ["same record: true"]);
    await expectRun("module entry in a cycle",
      ["cycle-a.mjs"], ["b ran", "a ran"]);
    await expectRun("--source-type=module entry",
      ["self.js", "--source-type=module"], ["self ran"]);

    // No explicit source type: a .js entry with an import declaration is a
    // Module (ADR 0140), so it gets the same single record as above.
    await expectRun("detected module entry importing itself",
      ["self.js"], ["self ran"]);
    await expectRun("detected module entry record identity",
      ["id.js"], ["same record: true"]);
    await expectRun("detected module entry in a cycle",
      ["cycle-a.js"], ["b ran", "a ran"]);

    // Script source: the entry is a Script Record (§16.1.4), not a Module
    // Record, so its own path names a separate module record. That module
    // evaluates once, before the script body, and its bindings are its own.
    await expectRun("script entry importing itself",
      ["self.js", "--source-type=script"], ["self ran", "self ran"]);
    await expectRun("script entry record identity",
      ["id.js", "--source-type=script"], ["same record: true", "same record: false"]);
    await expectRun("script entry in a cycle",
      ["cycle-a.js", "--source-type=script"], ["a ran", "b ran", "a ran"]);


    // A bundled module-source entry is the .gbc file: an import of that file
    // gets the running entry instead of reading the bytecode as source.
    const bundle = await $`${join(process.cwd(), BUNDLER)} bundled.mjs`.cwd(tmp).nothrow().quiet();
    if (bundle.exitCode !== 0) throw new Error(`Bundling bundled.mjs failed: ${bundle.text()}`);
    await expectRun("module .gbc entry importing itself", ["bundled.gbc", "--source-type=module"], ["bundled ran"]);
  } finally {
    clean(tmp);
  }
}

// -- Entry waits for a dependency's top-level await (#1652) --------------------

// ES2026 §16.2.1.6.1.3.1 InnerModuleEvaluation: a module the entry imports that
// evaluates asynchronously is one of the entry's pending dependencies, so the
// entry's body runs once it has evaluated (§16.2.1.6.1.3.4
// AsyncModuleExecutionFulfilled) and not at all if it fails (§16.2.1.6.1.3.5
// AsyncModuleExecutionRejected). tests/language/modules covers the order and a
// cycle; this covers the run's exit status, its --print result, and a
// dependency waiting on a timer.
console.log("Entry waits for a dependency's top-level await...");
{
  const tmp = mkdtemp("goccia-entry-tla-");
  try {
    const files: Record<string, string> = {
      "dep.mjs": 'console.log("dep start");\nexport let later = "before";\nawait 0;\nlater = "after";\nconsole.log("dep end");\n',
      "order.mjs": 'import { later } from "./dep.mjs";\nconsole.log("entry " + later);\n',
      "print.mjs": 'import { later } from "./dep.mjs";\n"completion " + later;\n',
      "failing-dep.mjs": 'export let later = "before";\nawait 0;\nthrow new Error("dependency failed");\n',
      "failing.mjs": 'import { later } from "./failing-dep.mjs";\nconsole.log("entry ran");\n',
      "timer-dep.mjs": 'export let later = "before";\nawait new Promise((resolve) => setTimeout(resolve, 5));\nlater = "after";\n',
      "timer.test.mjs": 'import { later } from "./timer-dep.mjs";\nconst atEntry = later;\ntest("the entry ran after the timer", () => { expect(atEntry).toBe("after"); });\n',
    };
    for (const [name, source] of Object.entries(files)) writeFileSync(join(tmp, name), source);

    const lines = (out: string, prefixes: string[]): string[] =>
      normalizeLineEndings(out)
        .split("\n")
        .filter((line) => prefixes.some((prefix) => line.startsWith(prefix)));

    const order = await $`${join(process.cwd(), RUNNER)} order.mjs`.cwd(tmp).nothrow().quiet();
    const orderLines = lines(order.text(), ["dep ", "entry "]);
    if (order.exitCode !== 0 || JSON.stringify(orderLines) !== JSON.stringify(["dep start", "dep end", "entry after"]))
      throw new Error(`entry order: got exit ${order.exitCode}: ${order.text()}${order.stderr}`);

    const failing = await $`${join(process.cwd(), RUNNER)} failing.mjs`.cwd(tmp).nothrow().quiet();
    const failingOut = failing.text() + failing.stderr.toString();
    if (failing.exitCode === 0 || failingOut.includes("entry ran") || !failingOut.includes("dependency failed"))
      throw new Error(`failing dependency: got exit ${failing.exitCode}: ${failingOut}`);

    const print = await $`${join(process.cwd(), RUNNER)} print.mjs --print`.cwd(tmp).nothrow().quiet();
    if (print.exitCode !== 0 || !lines(print.text(), ["completion "]).includes("completion after"))
      throw new Error(`--print result: got exit ${print.exitCode}: ${print.text()}${print.stderr}`);

    const timer = await $`${join(process.cwd(), TESTRUNNER)} -P timer.test.mjs`.cwd(tmp).nothrow().quiet();
    if (timer.exitCode !== 0)
      throw new Error(`timer dependency: got exit ${timer.exitCode}: ${timer.text()}${timer.stderr}`);
  } finally {
    clean(tmp);
  }
}

// -- Timers: containment and uncaught attribution ------------------------------

// Two properties that only show up in the runner's output, so neither can be
// asserted from inside a suite.
console.log("Timers (containment and uncaught attribution)...");
{
  const tmp = mkdtemp("goccia-timers-");
  try {
    // A timer callback that throws is an uncaught error in Node, not something
    // the frame that happened to be awaiting can catch. It must therefore leave
    // the await alone AND still fail the test that scheduled it — reporting it
    // at the await made an unrelated try/catch swallow it and left the awaited
    // promise pending on top of that.
    const uncaught = join(tmp, "uncaught.test.js");
    writeFileSync(
      uncaught,
      [
        'test("a throwing timer does not surface at an unrelated await", async () => {',
        "  let caughtHere = null;",
        "  setTimeout(() => { throw new Error('from-the-timer'); }, 0);",
        "  try {",
        "    const value = await new Promise((resolve) => setTimeout(() => resolve('resolved'), 5));",
        "    console.log('AWAIT-RESULT: ' + value);",
        "  } catch (error) {",
        "    caughtHere = error.message;",
        "  }",
        "  console.log('CAUGHT-AT-AWAIT: ' + caughtHere);",
        "});",
        "",
      ].join("\n"),
    );
    const out = (
      await $`${TESTRUNNER} ${uncaught} --no-progress 2>&1`.nothrow()
    ).text();

    if (!out.includes("AWAIT-RESULT: resolved"))
      throw new Error(
        `a throwing timer must not disturb the awaiting frame, got: ${out}`,
      );
    if (!out.includes("CAUGHT-AT-AWAIT: null"))
      throw new Error(
        `a throwing timer must not be catchable at the await, got: ${out}`,
      );
    if (!out.includes("uncaught exception in a timer callback"))
      throw new Error(
        `a throwing timer must fail the test that scheduled it, got: ${out}`,
      );
    if (!out.includes("from-the-timer"))
      throw new Error(`the timer's own error must be named, got: ${out}`);

    // The timer surface is the runner's alone. The loader gets neither the
    // globals nor the module, so a sandboxed script cannot schedule anything.
    const probe = join(tmp, "probe.js");
    writeFileSync(
      probe,
      [
        'console.log("setTimeout=" + typeof setTimeout);',
        'console.log("setInterval=" + typeof setInterval);',
        'console.log("clearTimeout=" + typeof clearTimeout);',
        "",
      ].join("\n"),
    );
    const loaderOut = (await $`${RUNNER} ${probe} 2>&1`.nothrow()).text();
    for (const absent of [
      "setTimeout=undefined",
      "setInterval=undefined",
      "clearTimeout=undefined",
    ]) {
      if (!loaderOut.includes(absent))
        throw new Error(
          `GocciaRunner must not expose the timer globals (${absent}), got: ${loaderOut}`,
        );
    }

    const moduleProbe = join(tmp, "module-probe.js");
    writeFileSync(
      moduleProbe,
      [
        'import * as timers from "goccia:timers";',
        "console.log(typeof timers);",
        "",
      ].join("\n"),
    );
    const moduleOut = (
      await $`${RUNNER} ${moduleProbe} --source-type=module 2>&1`.nothrow()
    ).text();
    if (moduleOut.includes("object"))
      throw new Error(
        `GocciaRunner must not resolve goccia:timers, got: ${moduleOut}`,
      );
  } finally {
    clean(tmp);
  }
}

// -- Global injection (TestRunner) ---------------------------------------------

// The runner grew --global/--globals so a suite can be handed a host global it
// needs. `process` is the case that forced it: GocciaScript has none, and
// vi.stubEnv has nowhere to write without one.
console.log("Global injection (TestRunner)...");
{
  const tmp = mkdtemp("goccia-testrunner-globals-");
  try {
    const suite = join(tmp, "globals.test.js");
    writeFileSync(
      suite,
      [
        'describe("injected", () => {',
        '  test("reads the injected global", () => {',
        '    expect(process.env.PRESET).toBe("from-host");',
        "  });",
        "});",
        "",
      ].join("\n"),
    );

    const inline = await $`${TESTRUNNER} ${suite} --no-progress --global ${'process={"env":{"PRESET":"from-host"}}'} 2>&1`.nothrow();
    if (inline.exitCode !== 0)
      throw new Error(`--global should inject process, got: ${inline.text()}`);

    const globalsFile = join(tmp, "env.json");
    writeFileSync(globalsFile, JSON.stringify({ process: { env: { PRESET: "from-host" } } }));
    const fromFile = await $`${TESTRUNNER} ${suite} --no-progress --globals=${globalsFile} 2>&1`.nothrow();
    if (fromFile.exitCode !== 0)
      throw new Error(`--globals should inject process, got: ${fromFile.text()}`);

    // Without the injection the suite must fail on the missing global rather
    // than quietly reading undefined.
    const without = await $`${TESTRUNNER} ${suite} --no-progress 2>&1`.nothrow();
    if (without.exitCode === 0)
      throw new Error("Suite should fail when process is not injected");
    if (!without.text().includes("process"))
      throw new Error(`Missing-global failure should name process, got: ${without.text()}`);
  } finally {
    clean(tmp);
  }
}

// -- Example scripts (Loader) ---------------------------------------------------

console.log("Example scripts...");
const stableExamples = [...new Bun.Glob("**/*.js").scanSync({ cwd: "examples" })]
  .filter((path) => !path.split("/").some((segment) => segment.startsWith("_")))
  .sort()
  .map((path) => join("examples", path));
await $`${RUNNER} ${stableExamples}`.quiet();

console.log("\nAll test-cli.ts tests passed.");
