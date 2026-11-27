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
 *     ./build.pas runner testrunner benchmarkrunner bundler repl loaderbare test262runner fuzzharness \
 *       tomlcompliancerunner
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
  fuzzHarness: faultBuild("GocciaFuzzHarness"),
  tomlRunner: faultBuild("GocciaTOMLComplianceRunner"),
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

/** Runs `command` (POSIX only) with stderr a pipe already full that nobody reads, so the fault's report blocks, and
 * stdout captured; with `blockAlarm`, SIGALRM arrives blocked, as a launcher may leave it. Prints `status=<code>`,
 * or `status=hung` when the process outlives 30 seconds. */
function runWithFullStderr(command: string[], env: Record<string, string> = {}, blockAlarm = false): Run {
  return run(["python3", "-I", "-c", [
    "import fcntl, os, signal, subprocess, sys",
    "r, w = os.pipe()",
    "fl = fcntl.fcntl(w, fcntl.F_GETFL)",
    "fcntl.fcntl(w, fcntl.F_SETFL, fl | os.O_NONBLOCK)",
    "try:",
    "    while True: os.write(w, b'x' * 4096)",
    "except BlockingIOError: pass",
    "fcntl.fcntl(w, fcntl.F_SETFL, fl)",
    `if ${blockAlarm ? "True" : "False"}: signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGALRM})`,
    "p = subprocess.Popen(sys.argv[1:], stdout=subprocess.PIPE, stderr=w)",
    "try:",
    "    out, _ = p.communicate(timeout=30)",
    "    sys.stdout.write(out.decode('utf-8', 'replace'))",
    "    print('status=%d' % p.returncode)",
    "except subprocess.TimeoutExpired:",
    "    p.kill(); p.wait(); print('status=hung')",
  ].join("\n"), ...command], { env });
}

/** Runs `command` (POSIX only) with stderr a pipe whose reader has gone, so the fault's report fails to write;
 * prints `status=<code>`. */
function runWithClosedStderr(command: string[], env: Record<string, string> = {}): Run {
  return run(["sh", "-c", `exec 3>&1; { sleep 1; "$@" 2>&1 >/dev/null; echo "status=$?" >&3; } | true`, "sh",
    ...command], { env });
}

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

await section("GocciaRunner: every fault kind inside the compile handler, a stack overflow included, stops the run", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "entry.js");
    writeFileSync(file, "console.log('ran');\n");
    for (const [kind, exceptionClass] of [
      ["stack-overflow", "EStackOverflow"],
      ["assertion", "EAssertionFailed"],
      ["invalid-pointer", "EInvalidPointer"],
    ]) {
      const r = run([F.runner, file], { env: { GOCCIA_FAULT_INJECTION: `${kind}@compile-entry` } });
      expectFault(r, exceptionClass, "entry.js");
      expectAbsent(r, "SyntaxError", "the fault was reported as a script's syntax error");
    }
    // a script's own deep recursion stays the catchable RangeError the engine bounds it with
    const deep = join(tmp, "deep.js");
    writeFileSync(deep, [
      "const down = (n) => 1 + down(n + 1);",
      "try { down(0); } catch (e) { console.log(e instanceof RangeError ? 'range error' : String(e)); }",
    ].join("\n"));
    const recursion = run([F.runner, deep]);
    expectNoFault(recursion, 0);
    if (!recursion.stdout.includes("range error")) throw new Error(`deep recursion: ${describe(recursion)}`);
  } finally {
    clean(tmp);
  }
});

await section("Every application: a fault in a worker's cleanup, as an engine is freed, or as the application ends, stops it", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "fine.js");
    writeFileSync(file, "console.log('fine');\n");
    const [test] = writeTestFiles(tmp, { "a.test.js": PASSING_TEST("A RAN") });
    expectFault(run([F.runner, file], { env: { GOCCIA_FAULT_INJECTION: "access-violation@thread-cleanup" } }),
      "EAccessViolation", "fine.js");
    const r = run([F.testRunner, "-P", test], { env: { GOCCIA_FAULT_INJECTION: "access-violation@thread-cleanup" } });
    expectFault(r, "EAccessViolation", "a.test.js");
    expectAbsent(r, "Test Results", "a summary was printed");
    expectFault(run([F.runner, file], { env: { GOCCIA_FAULT_INJECTION: "access-violation@shutdown" } }),
      "EAccessViolation", "GocciaRunner");
    // a host freeing an engine, in sandbox mode too, where the run's own thread frees it
    expectFault(run([F.runner, file], { env: { GOCCIA_FAULT_INJECTION: "access-violation@engine-free" } }),
      "EAccessViolation", "fine.js");
    mkdirSync(join(tmp, "tree"));
    writeFileSync(join(tmp, "tree", "main.js"), "console.log('fine');\n");
    expectFault(run([F.runner, "--copy", `${join(tmp, "tree")}=/`, "--entry=/main.js"],
      { env: { GOCCIA_FAULT_INJECTION: "access-violation@engine-free" } }), "EAccessViolation", "main.js");
    expectFault(run([F.testRunner, "-P", test], { env: { GOCCIA_FAULT_INJECTION: "assertion@shutdown" } }),
      "EAssertionFailed", "GocciaTestRunner");
  } finally {
    clean(tmp);
  }
});

await section("Every application: a failed engine integrity check stops it the same way, without unwinding", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "check.js");
    writeFileSync(file, "try { __gocciaInjectFault('engine-check'); } finally { console.log('finally'); }\n");
    for (const binary of [F.runner, F.bare]) {
      const r = run([binary, file]);
      const lines = r.stderr.split("\n").filter((line) => line.startsWith(PREFIX));
      if (r.code !== EXIT_ENGINE_FAULT || lines.length !== 2 ||
        lines[0] !== `${PREFIX}the engine's injected check failed (see above)` || lines[1] !== SECOND_LINE ||
        !r.stderr.includes("injected engine check: the state it found"))
        throw new Error(`expected the engine-check diagnostic and exit 70, got ${describe(r)}`);
      expectAbsent(r, "finally", "a finally block ran");
    }
  } finally {
    clean(tmp);
  }
});

await section("Every application: a broken stderr cannot change the exit status or hang the fault", () => {
  if (process.platform === "win32") return;
  const tmp = makeTmp();
  try {
    const fault = join(tmp, "fault.js");
    writeFileSync(fault, "__gocciaInjectFault('access-violation');\n");
    const check = join(tmp, "check.js");
    writeFileSync(check, "__gocciaInjectFault('engine-check');\n");
    for (const file of [fault, check]) {
      const closed = runWithClosedStderr([F.runner, file]);
      if (!closed.stdout.includes("status=70"))
        throw new Error(`a closed stderr pipe should still give exit 70 for ${file}, got ${describe(closed)}`);
      const full = runWithFullStderr([F.runner, file]);
      if (!full.stdout.includes("status=70"))
        throw new Error(`a full stderr pipe should end in exit 70 within the bound for ${file}, got ${describe(full)}`);
    }
    // a launcher can leave SIGALRM blocked: the watchdog unblocks it
    const masked = runWithFullStderr([F.runner, fault], {}, true);
    if (!masked.stdout.includes("status=70"))
      throw new Error(`a blocked SIGALRM should not disable the watchdog, got ${describe(masked)}`);
  } finally {
    clean(tmp);
  }
});

await section("GocciaTestRunner: no file starts while a fault's report is still being written", () => {
  if (process.platform === "win32") return;
  const tmp = makeTmp();
  try {
    // with stderr full, the faulting file's report blocks until the watchdog; the passing file ends meanwhile, and
    // the scheduler must not start the third file, which would print its mark at once
    for (const kind of ["access-violation", "engine-check"]) {
      const files = writeTestFiles(tmp, {
        "a.test.js": "test('busy', () => { const end = Date.now() + 800; while (Date.now() < end) {} });\n",
        "b.test.js": `test('faults', () => { __gocciaInjectFault('${kind}'); });\n`,
        "c.test.js": "test('marks', () => { __gocciaInjectFault('mark'); });\n",
      });
      const r = runWithFullStderr([F.testRunner, "-P", "--jobs=2", "--compat-while-loops", "--no-progress", ...files]);
      if (!r.stdout.includes("status=70") || r.stdout.includes("injected mark"))
        throw new Error(`expected exit 70 with the third file never started (${kind}), got ${describe(r)}`);
      // the control: a file that runs prints its mark
      const control = run([F.testRunner, "-P", "--jobs=2", "--compat-while-loops", "--no-progress", files[0], files[2]]);
      if (control.code !== 0 || !control.stdout.includes("injected mark"))
        throw new Error(`the mark should show when the file runs, got ${describe(control)}`);
    }
  } finally {
    clean(tmp);
  }
});

await section("GocciaTOMLComplianceRunner: a fault in the coordinator stops it", () => {
  expectFault(run([F.tomlRunner, "--suite-dir", "."], { env: STARTUP }), "EAccessViolation", "GocciaTOMLComplianceRunner");
});

await section("GocciaFuzzHarness: a fault is a finding, exit 1 with its class (the fuzzer's contract)", () => {
  const tmp = makeTmp();
  try {
    const file = join(tmp, "input.js");
    writeFileSync(file, "__gocciaInjectFault('access-violation');\n");
    const r = run([F.fuzzHarness, file]);
    if (r.code !== 1 || !/unexpected fault in the bytecode run: EAccessViolation/.test(r.stderr) || r.stderr.includes(PREFIX))
      throw new Error(`expected the harness's finding and exit 1, got ${describe(r)}`);
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
