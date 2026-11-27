#!/usr/bin/env bun
/**
 * test-engine-faults.ts
 *
 * Every application stops on a real engine fault: two `Engine fault:` lines on
 * stderr, exit 70, no summary or report, and nothing run after it
 * (docs/adr/0196-every-application-stops-on-an-engine-fault.md,
 * docs/contributing/cli-conventions.md#exit-codes).
 *
 * The faults come from the fault build, which no shipped target makes
 * (Goccia.FaultInjection):
 *
 *   GOCCIA_VARIANT=FaultInjection GOCCIA_FPC_FLAGS=-dGOCCIA_FAULT_INJECTION \
 *     ./build.pas runner testrunner benchmarkrunner bundler repl loaderbare test262runner
 *
 * writes build/GocciaRunnerFaultInjection and the others. The shipped binaries
 * in build/ are read too, to show that they carry no trace of the hook.
 *
 * Usage:
 *   bun run scripts/test-engine-faults.ts
 */

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { join, resolve } from "path";
import { BARE, BENCHRUNNER, BUNDLER, REPL, RUNNER, TEST262RUNNER, TESTRUNNER } from "./test-cli/binaries";
import { clean, makeTmpFactory } from "./test-cli/tmpdir";

const EXT = process.platform === "win32" ? ".exe" : "";
const faultBuild = (app: string) => resolve(`./build/${app}FaultInjection${EXT}`);
const F = {
  runner: faultBuild("GocciaRunner"),
  testRunner: faultBuild("GocciaTestRunner"),
  benchRunner: faultBuild("GocciaBenchmarkRunner"),
  bundler: faultBuild("GocciaBundler"),
  repl: faultBuild("GocciaREPL"),
  bare: faultBuild("GocciaScriptLoaderBare"),
  test262Runner: faultBuild("GocciaTest262Runner"),
};
const SHIPPED = [RUNNER, TESTRUNNER, BENCHRUNNER, BUNDLER, REPL, BARE, TEST262RUNNER];

const PREFIX = "Engine fault: ";
const SECOND_LINE = PREFIX +
  "the run was stopped because the engine can no longer vouch for its own state; no result of this run can be trusted.";
const EXIT_ENGINE_FAULT = 70;

const makeTmp = makeTmpFactory("goccia-faults-");
let failures = 0;

async function section(name: string, body: () => void | Promise<void>): Promise<void> {
  try {
    await body();
    console.log(`ok   ${name}`);
  } catch (error) {
    failures++;
    console.error(`FAIL ${name}\n     ${(error as Error).message.split("\n").join("\n     ")}`);
  }
}

type Run = { code: number | null; stdout: string; stderr: string };

function run(command: string[], options: { cwd?: string; env?: Record<string, string>; stdin?: string } = {}): Run {
  const result = Bun.spawnSync(command, {
    cwd: options.cwd,
    env: { ...process.env, NO_COLOR: "1", ...options.env },
    stdin: options.stdin === undefined ? "ignore" : Buffer.from(options.stdin),
    stdout: "pipe",
    stderr: "pipe",
    timeout: 60_000,
  });
  const text = (bytes: Uint8Array) => Buffer.from(bytes).toString("utf8").replace(/\r\n/g, "\n");
  return { code: result.exitCode, stdout: text(result.stdout), stderr: text(result.stderr) };
}

function describe(r: Run): string {
  return `exit ${r.code}\n--- stdout\n${r.stdout}\n--- stderr\n${r.stderr}`;
}

function escapeRegExp(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/** The fault contract: exit 70 and exactly the two diagnostic lines, the
 * first naming the exception's class and a site ending in `site`. */
function expectFault(r: Run, exceptionClass: string, site: string): void {
  const lines = r.stderr.split("\n").filter((line) => line.startsWith(PREFIX));
  const first = new RegExp(`^${escapeRegExp(PREFIX + exceptionClass)} in (?:.*[/\\\\])?${escapeRegExp(site)}: `);
  if (r.code !== EXIT_ENGINE_FAULT || lines.length !== 2 || !first.test(lines[0]) || lines[1] !== SECOND_LINE)
    throw new Error(`expected exit 70 and the ${exceptionClass} diagnostic naming ${site}, got ${describe(r)}`);
  if (/^Error:|^Fatal error:/m.test(r.stdout + r.stderr))
    throw new Error(`an engine fault was also reported as an ordinary error: ${describe(r)}`);
}

function expectAbsent(r: Run, text: string, what: string): void {
  if (r.stdout.includes(text) || r.stderr.includes(text))
    throw new Error(`${what} (${JSON.stringify(text)}) after the fault: ${describe(r)}`);
}

function expectNoFault(r: Run, code: number): void {
  if (r.code !== code || r.stderr.includes(PREFIX) || r.stdout.includes(PREFIX))
    throw new Error(`expected exit ${code} with no engine-fault diagnostic, got ${describe(r)}`);
}

const STARTUP = { GOCCIA_FAULT_INJECTION: "access-violation@startup" };

for (const path of Object.values(F)) {
  if (!existsSync(path)) {
    console.error(`missing ${path}: build the fault applications first (see the header of this script)`);
    process.exit(1);
  }
}

await section("shipped binaries carry no fault-injection hook, and the check can see one", () => {
  const needles = ["__gocciaInjectFault", "GOCCIA_FAULT_INJECTION"].flatMap((text) => [
    Buffer.from(text, "latin1"),
    Buffer.from(text, "utf16le"),
  ]);
  const has = (path: string) => {
    const bytes = readFileSync(path);
    return needles.some((needle) => bytes.includes(needle));
  };
  for (const binary of SHIPPED) {
    if (!existsSync(binary)) throw new Error(`missing shipped binary ${binary}`);
    if (has(binary)) throw new Error(`${binary} contains the fault-injection hook`);
  }
  for (const binary of Object.values(F))
    if (!has(binary)) throw new Error(`${binary} has no trace of the hook, so the check above proves nothing`);
});

await section("GocciaRunner: every fault kind stops the run with its class, after what the script printed", () => {
  const tmp = makeTmp();
  try {
    const kinds: Array<[string, string]> = [
      ["access-violation", "EAccessViolation"],
      ["assertion", "EAssertionFailed"],
      ["invalid-pointer", "EInvalidPointer"],
      ["stack-overflow", "EStackOverflow"],
    ];
    for (const [kind, exceptionClass] of kinds) {
      const file = join(tmp, `${kind}.js`);
      writeFileSync(file, `console.log("before");\n__gocciaInjectFault("${kind}");\nconsole.log("after");\n`);
      const r = run([F.runner, file]);
      expectFault(r, exceptionClass, `${kind}.js`);
      if (!r.stdout.includes("before")) throw new Error(`the output before the fault is lost: ${describe(r)}`);
      expectAbsent(r, "after", "the script went on");
    }
  } finally {
    clean(tmp);
  }
});

await section("GocciaRunner: no catch, finally, promise rejection handler or later job sees the fault", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "guarded.js");
    writeFileSync(file, [
      "try { __gocciaInjectFault('access-violation'); } catch (e) { console.log('caught'); }",
      "finally { console.log('finally'); }",
    ].join("\n"));
    let r = run([F.runner, file]);
    expectFault(r, "EAccessViolation", "guarded.js");
    expectAbsent(r, "caught", "a catch block ran");
    expectAbsent(r, "finally", "a finally block ran");

    const asyncFile = join(tmp, "async.js");
    writeFileSync(asyncFile, [
      "Promise.resolve().then(() => __gocciaInjectFault('access-violation')).catch(() => console.log('caught'));",
      "const later = async () => { try { await null; __gocciaInjectFault('access-violation'); }",
      "  catch (e) { console.log('async caught'); } finally { console.log('async finally'); } };",
      "later();",
      "Promise.resolve().then(() => Promise.resolve()).then(() => console.log('later job'));",
    ].join("\n"));
    r = run([F.runner, asyncFile]);
    expectFault(r, "EAccessViolation", "async.js");
    for (const text of ["caught", "async finally", "later job"]) expectAbsent(r, text, "a handler or later job ran");
  } finally {
    clean(tmp);
  }
});

await section("GocciaRunner: no JSON report, in sandbox mode too, and across parallel files", () => {
  const tmp = makeTmp();
  try {
    const fault = join(tmp, "fault.js");
    const fine = join(tmp, "fine.js");
    writeFileSync(fault, "console.log('before'); __gocciaInjectFault('access-violation');\n");
    writeFileSync(fine, "console.log('fine');\n");
    let r = run([F.runner, "--output=json", fault]);
    expectFault(r, "EAccessViolation", "fault.js");
    if (r.stdout.trim() !== "") throw new Error(`a JSON report was written: ${describe(r)}`);

    r = run([F.runner, "--output=json", "--jobs=2", fine, fault, fine]);
    expectFault(r, "EAccessViolation", "fault.js");
    if (r.stdout.trim() !== "") throw new Error(`a JSON report was written: ${describe(r)}`);

    mkdirSync(join(tmp, "tree"));
    writeFileSync(join(tmp, "tree", "main.js"), "__gocciaInjectFault('access-violation');\n");
    r = run([F.runner, "--copy", `${join(tmp, "tree")}=/`, "--entry=/main.js"]);
    expectFault(r, "EAccessViolation", "main.js");
  } finally {
    clean(tmp);
  }
});

await section("GocciaRunner: a fault inside the engine's own compile handler is not a SyntaxError", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "entry.js");
    writeFileSync(file, "console.log('ran');\n");
    const r = run([F.runner, file], { env: { GOCCIA_FAULT_INJECTION: "access-violation@compile-entry" } });
    expectFault(r, "EAccessViolation", "entry.js");
    expectAbsent(r, "SyntaxError", "the fault was reported as a script's syntax error");
    expectAbsent(r, "ran", "the entry ran");
  } finally {
    clean(tmp);
  }
});

await section("GocciaRunner: a fault at startup, outside any file, names the application", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "fine.js");
    writeFileSync(file, "console.log('fine');\n");
    const r = run([F.runner, file], { env: STARTUP });
    expectFault(r, "EAccessViolation", "GocciaRunner");
    expectAbsent(r, "fine", "the file ran");
  } finally {
    clean(tmp);
  }
});

await section("GocciaRunner: a script error and an unknown fault kind stay ordinary errors (exit 1)", () => {
  const tmp = makeTmp();
  try {
    const thrown = join(tmp, "throws.js");
    writeFileSync(thrown, "throw new TypeError('ordinary');\n");
    expectNoFault(run([F.runner, thrown]), 1);
    const unknown = join(tmp, "unknown.js");
    writeFileSync(unknown, "__gocciaInjectFault('no-such-kind');\n");
    expectNoFault(run([F.runner, unknown]), 1);
  } finally {
    clean(tmp);
  }
});

function writeTestFiles(dir: string, files: Record<string, string>): string[] {
  return Object.entries(files).map(([name, body]) => {
    const path = join(dir, name);
    writeFileSync(path, body);
    return path;
  });
}

const FAULTING_TEST = [
  "test('faults', () => {",
  "  try { __gocciaInjectFault('access-violation'); } catch (e) { console.log('test caught'); }",
  "});",
  "test('after the fault', () => { console.log('LATER TEST RAN'); });",
].join("\n");
const PASSING_TEST = (marker: string) => `test('passes', () => { console.log('${marker}'); expect(1).toBe(1); });\n`;

await section("GocciaTestRunner: a fault stops the run at that file, with no summary (--jobs=1)", () => {
  const tmp = makeTmp();
  try {
    const files = writeTestFiles(tmp, {
      "a.test.js": PASSING_TEST("A RAN"),
      "b.test.js": FAULTING_TEST,
      "c.test.js": PASSING_TEST("C RAN"),
    });
    const r = run([F.testRunner, "-P", "--jobs=1", ...files]);
    expectFault(r, "EAccessViolation", "b.test.js");
    for (const text of ["test caught", "LATER TEST RAN", "C RAN", "Test Results"])
      expectAbsent(r, text, "the run went on");
  } finally {
    clean(tmp);
  }
});

await section("GocciaTestRunner: parallel workers, JSON output and two faulting files give one diagnostic", () => {
  const tmp = makeTmp();
  try {
    const files = writeTestFiles(tmp, {
      "a.test.js": PASSING_TEST("A RAN"),
      "b.test.js": FAULTING_TEST,
      "c.test.js": PASSING_TEST("C RAN"),
      "d.test.js": PASSING_TEST("D RAN"),
    });
    let r = run([F.testRunner, "-P", "--jobs=2", ...files]);
    expectFault(r, "EAccessViolation", "b.test.js");
    expectAbsent(r, "Test Results", "a summary was printed");

    r = run([F.testRunner, "-P", "--jobs=2", "--output=json", ...files]);
    expectFault(r, "EAccessViolation", "b.test.js");
    if (r.stdout.trim() !== "") throw new Error(`a JSON report was written: ${describe(r)}`);

    const twice = writeTestFiles(tmp, { "e.test.js": FAULTING_TEST, "f.test.js": FAULTING_TEST });
    for (let attempt = 0; attempt < 5; attempt++) {
      r = run([F.testRunner, "-P", "--jobs=2", ...twice]);
      const lines = r.stderr.split("\n").filter((line) => line.startsWith(PREFIX));
      if (r.code !== EXIT_ENGINE_FAULT || lines.length !== 2 || lines[1] !== SECOND_LINE ||
        !/^Engine fault: EAccessViolation in .*[ef]\.test\.js: /.test(lines[0]))
        throw new Error(`two faulting files should give one diagnostic, got ${describe(r)}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("GocciaTestRunner: a fault at startup, and a failing test stays exit 1 with its summary", () => {
  const tmp = makeTmp();
  try {
    const [passing] = writeTestFiles(tmp, { "a.test.js": PASSING_TEST("A RAN") });
    const r = run([F.testRunner, "-P", passing], { env: STARTUP });
    expectFault(r, "EAccessViolation", "GocciaTestRunner");
    expectAbsent(r, "A RAN", "the file ran");

    const [failing] = writeTestFiles(tmp, { "fails.test.js": "test('fails', () => { expect(1).toBe(2); });\n" });
    const failed = run([F.testRunner, "-P", failing]);
    expectNoFault(failed, 1);
    if (!failed.stdout.includes("Test Results")) throw new Error(`no summary for a failing test: ${describe(failed)}`);
  } finally {
    clean(tmp);
  }
});

await section("GocciaBenchmarkRunner: a fault in a benchmark stops the run with no summary", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "fault.bench.js");
    writeFileSync(file, [
      'import { bench } from "goccia:microbench";',
      "bench('faults', () => { __gocciaInjectFault('access-violation'); });",
    ].join("\n"));
    let r = run([F.benchRunner, "--source-type=module", "--no-progress", file]);
    expectFault(r, "EAccessViolation", "fault.bench.js");
    expectAbsent(r, "Benchmark Summary", "a summary was printed");
    r = run([F.benchRunner, "--source-type=module", "--no-progress", file], { env: STARTUP });
    expectFault(r, "EAccessViolation", "GocciaBenchmarkRunner");
  } finally {
    clean(tmp);
  }
});

await section("GocciaBundler: a fault while compiling stops the run and writes no bytecode", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "entry.js");
    const out = join(tmp, "entry.gbc");
    writeFileSync(file, "console.log('compiled');\n");
    let r = run([F.bundler, file, `--output=${out}`], { env: { GOCCIA_FAULT_INJECTION: "access-violation@compile" } });
    expectFault(r, "EAccessViolation", "entry.js");
    if (existsSync(out)) throw new Error(`the bundler wrote ${out} after the fault`);
    r = run([F.bundler, file, `--output=${out}`], { env: STARTUP });
    expectFault(r, "EAccessViolation", "GocciaBundler");
  } finally {
    clean(tmp);
  }
});

await section("GocciaREPL: a fault in an input ends the session before the next input", () => {
  const r = run([F.repl], { stdin: "6 * 7\n__gocciaInjectFault('access-violation')\n'next input'\n" });
  expectFault(r, "EAccessViolation", "REPL");
  if (!r.stdout.includes("42")) throw new Error(`the input before the fault printed nothing: ${describe(r)}`);
  expectAbsent(r, "next input", "the session went on");
  expectFault(run([F.repl], { stdin: "1\n", env: STARTUP }), "EAccessViolation", "GocciaREPL");
});

await section("GocciaScriptLoaderBare: a fault in the script, and at startup", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "bare.js");
    writeFileSync(file, "__gocciaInjectFault('assertion');\n");
    expectFault(run([F.bare, file]), "EAssertionFailed", "bare.js");
    expectFault(run([F.bare, file], { env: STARTUP }), "EAccessViolation", "GocciaScriptLoaderBare");
  } finally {
    clean(tmp);
  }
});

await section("GocciaTest262Runner: a fault in the coordinator, outside every case", () => {
  expectFault(run([F.test262Runner, "--help"], { env: STARTUP }), "EAccessViolation", "GocciaTest262Runner");
});

if (failures > 0) {
  console.error(`\n${failures} engine-fault section(s) failed`);
  process.exit(1);
}
console.log("\nevery application stops on an engine fault");
