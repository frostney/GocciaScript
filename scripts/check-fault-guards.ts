#!/usr/bin/env bun
/**
 * Fails on a catch-all exception handler that could turn an engine fault into
 * an ordinary result (docs/adr/0196-every-application-stops-on-an-engine-fault.md).
 *
 * A catch-all is an `on E: Exception do` arm, or an `except` block with no
 * `on` arms. Each one in source/engine, source/units and source/app (Pascal
 * tests aside) must do one of these:
 *
 * - re-raise unconditionally (`raise;` as a statement of its own, the
 *   free-and-re-raise cleanup handlers);
 * - test the exception with `IsEngineIntegrityFault` or `IsEngineFault`, or
 *   stop the process with `AbortOnEngineFault`;
 * - carry a `fault-guard:` comment in the handler, or on the lines just above
 *   its `on` or `except`, saying why a fault there is not reported as an
 *   ordinary result (a fuzz harness that counts it as a finding, a
 *   conformance case graded as a crash).
 *
 * The check reads the code's layout, which ./format.pas keeps regular: a
 * handler's `except` and its closing `end` share an indentation.
 *
 * Usage:
 *   bun run scripts/check-fault-guards.ts
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
const GUARD = /\b(IsEngineIntegrityFault|IsEngineFault|AbortOnEngineFault)\s*\(/;
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

// Whether the handler lines re-raise unconditionally: a `raise;` whose line is
// not the branch of an `if`, `else` or `do` on the line before it.
function reraises(lines: string[]): boolean {
  for (let i = 0; i < lines.length; i++) {
    const text = code(lines[i]);
    if (!/^raise\s*;/.test(text) && !/\braise\s*;\s*$/.test(text)) continue;
    if (/\b(then|else|do)\s+raise\s*;/i.test(text)) continue;
    let j = i - 1;
    while (j >= 0 && code(lines[j]) === "") j--;
    if (j >= 0 && /\b(then|else|do)$/i.test(code(lines[j]))) continue;
    return true;
  }
  return false;
}

function check(path: string, findings: Finding[]): void {
  const raw = readFileSync(join(ROOT, path), "utf8");
  const rawLines = raw.split("\n");
  const lines = stripComments(raw).split("\n");
  for (let i = 0; i < lines.length; i++) {
    if (!/^except\b/i.test(lines[i].trim()) || code(lines[i]) !== "except") continue;
    const depth = indentation(lines[i]);
    let end = i + 1;
    while (end < lines.length &&
      !(indentation(lines[end]) === depth && /^end\b/i.test(lines[end].trim()))) end++;
    const body = lines.slice(i + 1, end);
    const rawBody = rawLines.slice(i + 1, end);
    const above = rawLines.slice(Math.max(0, i - 3), i);
    const firstCode = body.map(code).find((text) => text !== "");
    if (firstCode === undefined || !ANY_ARM.test(firstCode)) {
      // a bare handler: everything is caught
      if (!reraises(body) && !GUARD.test(body.join("\n")) &&
        !EXEMPTION.test([...above, ...rawBody].join("\n"))) {
        findings.push({ file: path, line: i + 1, message: "bare except handler swallows engine faults" });
      }
      continue;
    }
    // the arms, each to the next arm at its own indentation
    const arms: number[] = [];
    for (let k = 0; k < body.length; k++) {
      if (ANY_ARM.test(code(body[k]))) {
        if (arms.length === 0 || indentation(body[k]) === indentation(body[arms[0]])) arms.push(k);
      }
    }
    // the `else` arm of an except block catches everything the `on` arms did not
    for (let a = 0; a < arms.length; a++) {
      const start = arms[a];
      const stop = a + 1 < arms.length ? arms[a + 1] : body.length;
      if (!CATCH_ALL_ARM.test(code(body[start]))) continue;
      const arm = body.slice(start, stop);
      const context = [...rawBody.slice(Math.max(0, start - 3), stop)];
      if (start < 3) context.unshift(...above);
      if (reraises(arm.slice(1)) || GUARD.test(arm.join("\n")) || EXEMPTION.test(context.join("\n"))) continue;
      findings.push({ file: path, line: i + 2 + start, message: "catch-all arm swallows engine faults" });
    }
  }
}

const findings: Finding[] = [];
for (const tree of TREES) for (const file of sourceFiles(tree)) check(file, findings);
for (const f of findings) console.error(`${f.file}:${f.line}: ${f.message}`);
if (findings.length > 0) {
  console.error(`\n${findings.length} handler(s) without a fault guard. Test the exception with ` +
    "IsEngineIntegrityFault (a script boundary: re-raise it) or IsEngineFault (a host: AbortOnEngineFault), " +
    "or say why not in a `fault-guard:` comment. See docs/adr/0196-every-application-stops-on-an-engine-fault.md.");
  process.exit(1);
}
console.log(`fault guards: every catch-all handler in ${TREES.join(", ")} is guarded`);
