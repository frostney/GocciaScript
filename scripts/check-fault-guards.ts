#!/usr/bin/env bun
/**
 * Fails on a catch-all exception handler that could turn an engine fault into
 * an ordinary result (docs/adr/0198-every-application-stops-on-an-engine-fault.md).
 *
 * A catch-all is an `on E: Exception do` arm, or an `except` block with no
 * `on` arms. Each one in source/engine, source/units and source/app (Pascal
 * tests aside) must do one of these:
 *
 * - re-raise unconditionally (`raise;` as a statement of its own, the
 *   free-and-re-raise cleanup handlers);
 * - test the exception with `IsEngineFault`, or stop the process with
 *   `AbortOnEngineFault`;
 * - carry a `fault-guard:` comment in the handler, or on the lines just above
 *   its `on` or `except`, saying why a fault there is not reported as an
 *   ordinary result (a fuzz harness that counts it as a finding, a
 *   conformance case graded as a crash).
 *
 * The check reads the code's layout, which ./format.pas keeps regular: a
 * handler's `except` and its closing `end` share an indentation.
 *
 * A catch-all arm is an `on ...: Exception do` arm or an except block's
 * `else` arm; a re-raise counts only as a statement at the handler's own
 * level, not one nested in an `if` or an inner block.
 *
 * Usage:
 *   bun run scripts/check-fault-guards.ts
 *   bun run scripts/check-fault-guards.ts --self-test
 */

import { readFileSync, readdirSync, statSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const TREES = ["source/engine", "source/units", "source/app"];
const SKIPPED_DIRECTORIES = new Set(["source/engine/tools"]);
// the engine's own development programs (build/engine/goccia and its threads
// probe), which are not applications under source/app
const SKIPPED_FILES = new Set(["source/engine/goccia.dpr", "source/engine/gocciathreads.dpr"]);
const GUARD = /\b(IsEngineFault|AbortOnEngineFault)\s*\(/;
const EXEMPTION = /fault-guard:/;
const CATCH_ALL_ARM = /^on\s+(?:\w+\s*:\s*)?Exception\s+do\b/i;
const ANY_ARM = /^on\s+(?:\w+\s*:\s*)?[\w.]+\s+do\b/i;

type Finding = { file: string; line: number; message: string };

function sourceFiles(directory: string): string[] {
  const out: string[] = [];
  for (const name of readdirSync(join(ROOT, directory))) {
    const path = join(directory, name);
    if (statSync(join(ROOT, path)).isDirectory()) {
      if (!SKIPPED_DIRECTORIES.has(path)) out.push(...sourceFiles(path));
    } else if (/\.(pas|dpr|inc)$/.test(name) && !name.endsWith(".Test.pas") && !SKIPPED_FILES.has(path)) {
      out.push(path);
    }
  }
  return out;
}

function indentation(line: string): number {
  return line.length - line.trimStart().length;
}

// The source with every comment blanked out and its line breaks kept, so that
// line numbers still match: `{ }` (compiler directives too, which this check
// has no use for), `(* *)` and `//`. String literals are skipped over.
function stripComments(source: string): string {
  let out = "";
  let i = 0;
  const blank = (text: string) => text.replace(/[^\n]/g, " ");
  while (i < source.length) {
    const c = source[i];
    if (c === "'") {
      const close = source.indexOf("'", i + 1);
      const stop = close < 0 ? source.length : close + 1;
      out += source.slice(i, stop);
      i = stop;
    } else if (c === "{") {
      const close = source.indexOf("}", i);
      const stop = close < 0 ? source.length : close + 1;
      out += blank(source.slice(i, stop));
      i = stop;
    } else if (c === "(" && source[i + 1] === "*") {
      const close = source.indexOf("*)", i + 2);
      const stop = close < 0 ? source.length : close + 2;
      out += blank(source.slice(i, stop));
      i = stop;
    } else if (c === "/" && source[i + 1] === "/") {
      const close = source.indexOf("\n", i);
      const stop = close < 0 ? source.length : close;
      out += blank(source.slice(i, stop));
      i = stop;
    } else {
      out += c;
      i++;
    }
  }
  return out;
}

function code(line: string): string {
  return line.trim();
}

// Whether a handler body re-raises unconditionally: a `raise;` statement at
// the body's own level, not one nested in an `if`, a loop or an inner block.
// `inline` is the code after an arm's `do` on its own line, if any.
// A bare handler's statements: any `raise;` at the level of its first one.
function bareReraises(lines: string[]): boolean {
  const first = lines.findIndex((line) => code(line) !== "");
  if (first < 0) return false;
  const level = indentation(lines[first]);
  return lines.some((line) => indentation(line) === level && /^raise\s*;/i.test(code(line)));
}

function reraises(lines: string[], inline = ""): boolean {
  if (/^raise\s*;/i.test(inline)) return true;
  if (inline !== "") return false;
  const first = lines.findIndex((line) => code(line) !== "");
  if (first < 0) return false;
  let level = indentation(lines[first]);
  let statements = lines.slice(first);
  if (/^begin$/i.test(code(lines[first]))) {
    level += 2;
    statements = statements.slice(1);
  } else {
    // a single statement after `do`
    return /^raise\s*;/i.test(code(lines[first]));
  }
  return statements.some((line) => indentation(line) === level && /^raise\s*;/i.test(code(line)));
}

type Source = { path: string; text: string };

function check(source: Source, findings: Finding[]): void {
  const path = source.path;
  const rawLines = source.text.split("\n");
  const lines = stripComments(source.text).split("\n");
  for (let i = 0; i < lines.length; i++) {
    if (code(lines[i]).toLowerCase() !== "except") continue;
    const depth = indentation(lines[i]);
    let end = i + 1;
    while (end < lines.length &&
      !(indentation(lines[end]) === depth && /^end\b/i.test(lines[end].trim()))) end++;
    const body = lines.slice(i + 1, end);
    const rawBody = rawLines.slice(i + 1, end);
    const above = rawLines.slice(Math.max(0, i - 3), i);
    const firstIndex = body.findIndex((line) => code(line) !== "");
    if (firstIndex < 0 || !ANY_ARM.test(code(body[firstIndex]))) {
      // a bare handler: everything is caught
      if (!bareReraises(body) && !GUARD.test(body.join("\n")) &&
        !EXEMPTION.test([...above, ...rawBody].join("\n"))) {
        findings.push({ file: path, line: i + 1, message: "bare except handler swallows engine faults" });
      }
      continue;
    }
    // the arms, each to the next arm at their indentation; an `else` there is
    // an arm too, and catches everything the `on` arms did not
    const armIndent = indentation(body[firstIndex]);
    const arms: number[] = [];
    for (let k = firstIndex; k < body.length; k++) {
      const at = indentation(body[k]);
      if (at === armIndent && ANY_ARM.test(code(body[k]))) arms.push(k);
      else if ((at === armIndent || at === depth) && /^else\b/i.test(code(body[k]))) arms.push(k);
    }
    for (let a = 0; a < arms.length; a++) {
      const start = arms[a];
      const stop = a + 1 < arms.length ? arms[a + 1] : body.length;
      const head = code(body[start]);
      const isElse = /^else\b/i.test(head);
      if (!isElse && !CATCH_ALL_ARM.test(head)) continue;
      const inline = isElse ? head.replace(/^else\b/i, "").trim() : head.replace(/^.*?\bdo\b/i, "").trim();
      const arm = body.slice(start, stop);
      const context = [...rawBody.slice(Math.max(0, start - 3), stop)];
      if (start < 3) context.unshift(...above);
      if (reraises(arm.slice(1), inline) || GUARD.test(arm.join("\n")) || EXEMPTION.test(context.join("\n"))) continue;
      findings.push({
        file: path,
        line: i + 2 + start,
        message: isElse ? "except-else arm swallows engine faults" : "catch-all arm swallows engine faults",
      });
    }
  }
}

// Handlers the check must flag, and handlers it must accept.
function selfTest(): number {
  const flagged: Record<string, string> = {
    "unguarded catch-all": "  try\n    X;\n  except\n    on E: Exception do\n      Y := 1;\n  end;\n",
    "unguarded catch-all block": "  try\n    X;\n  except\n    on E: Exception do\n    begin\n      Y := 1;\n    end;\n  end;\n",
    "nameless catch-all": "  try\n    X;\n  except\n    on Exception do\n      Exit(False);\n  end;\n",
    "bare handler": "  try\n    X;\n  except\n    Y := 1;\n  end;\n",
    "except-else arm": "  try\n    X;\n  except\n    on E: EConvertError do\n      raise;\n  else\n    Y := 1;\n  end;\n",
    "raise nested in a block": "  try\n    X;\n  except\n    on E: Exception do\n    begin\n      if Z then\n      begin\n        raise;\n      end;\n      Y := 1;\n    end;\n  end;\n",
    "raise after then": "  try\n    X;\n  except\n    on E: Exception do\n    begin\n      if Z then\n        raise;\n      Y := 1;\n    end;\n  end;\n",
    "raise inline after then": "  try\n    X;\n  except\n    Y := 1;\n    if Z then raise;\n  end;\n",
  };
  const accepted: Record<string, string> = {
    "guarded catch-all": "  try\n    X;\n  except\n    on E: Exception do\n    begin\n      if IsEngineFault(E) then\n        raise;\n      Y := 1;\n    end;\n  end;\n",
    "free and re-raise": "  try\n    X;\n  except\n    Y.Free;\n    raise;\n  end;\n",
    "inline re-raise": "  try\n    X;\n  except\n    on E: Exception do raise;\n  end;\n",
    "single-statement re-raise": "  try\n    X;\n  except\n    on E: Exception do\n      raise;\n  end;\n",
    "specific arm only": "  try\n    X;\n  except\n    on E: EConvertError do\n      Y := 1;\n  end;\n",
    "exempted": "  try\n    X;\n  except\n    { fault-guard: graded as a crash }\n    on E: Exception do\n      Y := 1;\n  end;\n",
    "guarded else": "  try\n    X;\n  except\n    on E: EConvertError do\n      Y := 1;\n  else\n    raise;\n  end;\n",
  };
  let failures = 0;
  for (const [name, text] of Object.entries(flagged)) {
    const found: Finding[] = [];
    check({ path: name, text }, found);
    if (found.length !== 1) {
      console.error(`self-test: "${name}" should be flagged once, got ${found.length}`);
      failures++;
    }
  }
  for (const [name, text] of Object.entries(accepted)) {
    const found: Finding[] = [];
    check({ path: name, text }, found);
    if (found.length !== 0) {
      console.error(`self-test: "${name}" should pass, got ${found.map((f) => f.message).join(", ")}`);
      failures++;
    }
  }
  return failures;
}

if (process.argv.includes("--self-test")) {
  const failures = selfTest();
  if (failures > 0) process.exit(1);
  console.log("fault guards: self-test passed");
  process.exit(0);
}

const findings: Finding[] = [];
for (const tree of TREES)
  for (const file of sourceFiles(tree)) check({ path: file, text: readFileSync(join(ROOT, file), "utf8") }, findings);
for (const f of findings) console.error(`${f.file}:${f.line}: ${f.message}`);
if (findings.length > 0) {
  console.error(`\n${findings.length} handler(s) without a fault guard. Test the exception with IsEngineFault and ` +
    "re-raise it (a script boundary) or stop with AbortOnEngineFault (a host, or a thread that would end silently), " +
    "or say why neither in a `fault-guard:` comment. See docs/adr/0198-every-application-stops-on-an-engine-fault.md.");
  process.exit(1);
}
console.log(`fault guards: every catch-all handler in ${TREES.join(", ")} is guarded`);
