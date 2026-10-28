#!/usr/bin/env npx tsx
/**
 * check-vm-dispatch-layout.ts
 *
 * Keeps the bytecode VM's dispatch layout as measured (issue #1917).
 *
 * FPC lays out the arms of a case statement in source order. The arms of
 * the dispatch case in source/units/Goccia.VM.DispatchCase.inc that the AWFY
 * workloads execute most come first, in HOT_ARMS order, so that an edit to any
 * other arm cannot move them. ExecuteClosureRegistersInternal is compiled with
 * {$CODEALIGN JUMP=64}, so every arm starts on a 64-byte boundary. Both
 * changed how the binary performs, and both were measured, so a change to
 * either is a measured change too: run the layout A/B in "Dispatch layout" in
 * docs/bytecode-vm.md, then update HOT_ARMS with the arms.
 *
 * The check fails when:
 *   - the case does not start with the hot-arm marker comment and HOT_ARMS,
 *     in this order, followed by the cold-arm marker comment;
 *   - an opcode labels two arms;
 *   - Goccia.VM.pas no longer wraps ExecuteClosureRegistersInternal in
 *     {$PUSH}{$CODEALIGN JUMP=64} ... {$POP}.
 *
 * --rank ranks the arms from opcode profiles, one JSON file per workload, as
 * the AWFY driver writes them, and prints the arms whose mean share of executed
 * opcodes is at least --threshold (default 0.001), highest first, in HOT_ARMS
 * form. HOT_ARMS is that list for twelve pinned AWFY benchmarks, all but CD,
 * which GocciaScript does not parse, and Havlak:
 *
 *   node scripts/awfy-driver.js --awfy-dir <awfy>/benchmarks/JavaScript \
 *     --engines goccia --repetitions 1 --profile opcodes --profile-dir <dir> \
 *     --benchmark Bounce:200 --benchmark DeltaBlue:5000 --benchmark Json:20 \
 *     --benchmark List:500 --benchmark Mandelbrot:500 --benchmark NBody:50000 \
 *     --benchmark Permute:200 --benchmark Queens:400 --benchmark Richards:10 \
 *     --benchmark Sieve:1000 --benchmark Storage:100 --benchmark Towers:150
 *   npx tsx scripts/check-vm-dispatch-layout.ts --rank <dir>
 *
 * Usage:
 *   npx tsx scripts/check-vm-dispatch-layout.ts
 *   npx tsx scripts/check-vm-dispatch-layout.ts --self-test
 *   npx tsx scripts/check-vm-dispatch-layout.ts --rank <dir> [--threshold <share>]
 */

import { readFileSync, readdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const DISPATCH_CASE = join(ROOT, "source", "units", "Goccia.VM.DispatchCase.inc");
const VM_UNIT = join(ROOT, "source", "units", "Goccia.VM.pas");

// The hot arms, each as the labels it lists, highest mean share of executed
// opcodes over the AWFY workloads first (opcode profiles of the pinned AWFY
// corpus, issue #1917), except OP_ITER_NEXT, which comes first: FPC places
// the dispatch function's temporaries in the order it compiles the arms, and
// in frequency order the function's stack frame grew by 336 bytes on AArch64,
// so fewer native re-entries fit on the stack. With OP_ITER_NEXT first it is
// smaller than with the old order on AArch64 and x86-64. --rank prints the
// frequency order; move OP_ITER_NEXT, or whichever arm then keeps the frame
// small, to the front, and compare the frame of `ExecuteClosureRegistersInternal`
// (`sub sp, sp, #...` on AArch64, `lea -...(%rsp),%rsp` on x86-64) with main's.
export const HOT_ARMS = [
  "OP_ITER_NEXT",
  "OP_GET_PROP_CONST, OP_GET_LOCAL_PROP_CONST",
  "OP_GET_LOCAL",
  "OP_JUMP_IF_FALSE",
  "OP_JUMP",
  "OP_MOVE",
  "OP_CALL_METHOD",
  "OP_LOAD_INT",
  "OP_RETURN",
  "OP_SET_PROP_CONST",
  "OP_JUMP_IF_NOT_LT_IMM, OP_JUMP_IF_NOT_GT_IMM, OP_JUMP_IF_NOT_LTE_IMM, OP_JUMP_IF_NOT_GTE_IMM, OP_JUMP_IF_NOT_STRICT_EQ_IMM, OP_JUMP_IF_STRICT_EQ_IMM",
  "OP_ADD",
  "OP_ADD_FLOAT",
  "OP_ARRAY_SET",
  "OP_ARRAY_GET",
  "OP_MUL_FLOAT",
  "OP_LOAD_UNDEFINED",
  "OP_MUL",
  "OP_JUMP_IF_NOT_NULLISH",
  "OP_ADD_NUM_IMM",
  "OP_SUB",
  "OP_JUMP_IF_NULLISH",
  "OP_LOAD_FALSE",
  "OP_JUMP_IF_NOT_LT",
  "OP_JUMP_IF_TRUE",
  "OP_SUB_NUM_IMM",
  "OP_LTE",
  "OP_EQ",
  "OP_SET_LOCAL",
  "OP_LOAD_CHAR",
  "OP_LOAD_HOLE",
  "OP_ADD_INT",
  "OP_SUB_FLOAT",
  "OP_LOAD_CONST",
  "OP_GET_GLOBAL, OP_GET_GLOBAL_DECLARED",
  "OP_HAS_GLOBAL",
  "OP_GET_UPVALUE",
  "OP_LOAD_NULL",
  "OP_LOAD_TRUE",
  "OP_CONSTRUCT",
  "OP_PUSH_FINALLY_HANDLER",
  "OP_POP_HANDLER",
  "OP_BAND",
  "OP_CALL",
  "OP_GTE",
  "OP_NEQ",
  "OP_MOD",
  "OP_NOT",
  "OP_DIV",
];

const HOT_MARKER = "// Hot arms.";
const COLD_MARKER = "// Cold arms:";

type Item = { kind: "arm"; labels: string[]; line: number } | { kind: "hot" | "cold"; line: number };

// The case's top-level items in order: arm label lists (six-space indent, one
// or more lines ending in ':') and the two marker comments.
export const caseItems = (source: string): Item[] => {
  const lines = source.split(/\r?\n/);
  const start = lines.findIndex((line) => /^\s*case TGocciaOpCode\(Op\) of\s*$/.test(line));
  if (start < 0) throw new Error("dispatch case `case TGocciaOpCode(Op) of` not found");
  // The case's own else is the line `else` alone at its indent that the
  // OP_CALL_SELF comment follows; the arms hold other such lines.
  let end = -1;
  for (let index = start + 1; index < lines.length - 1; index++) {
    if (lines[index] === "        else" && /^\s*\/\/ OP_CALL_SELF /.test(lines[index + 1])) {
      end = index;
      break;
    }
  }
  if (end < 0) throw new Error("the dispatch case's else, followed by the OP_CALL_SELF comment, not found");
  const items: Item[] = [];
  let pending: string[] | null = null;
  for (let index = start + 1; index < end; index++) {
    const line = lines[index];
    if (line.startsWith(`      ${HOT_MARKER}`)) items.push({ kind: "hot", line: index + 1 });
    else if (line.startsWith(`      ${COLD_MARKER}`)) items.push({ kind: "cold", line: index + 1 });
    if (!/^ {6}OP_/.test(line)) {
      pending = null;
      continue;
    }
    // A label list, ending in `,` when it goes on, or in `:` and maybe the
    // arm's statement.
    const label = /^ {6}((?:OP_[A-Z0-9_]+\s*,\s*)*OP_[A-Z0-9_]+)\s*(,\s*$|:(?!=))/.exec(line);
    if (!label) throw new Error(`line ${index + 1}: arm label not understood: ${line.trim()}`);
    const names = label[1].split(",").map((name) => name.trim()).filter(Boolean);
    pending = pending ? pending.concat(names) : names;
    if (label[2] === ":") {
      items.push({ kind: "arm", labels: pending, line: index + 1 });
      pending = null;
    }
  }
  return items;
};

export const checkCase = (source: string, hotArms: string[] = HOT_ARMS): string[] => {
  const problems: string[] = [];
  const items = caseItems(source);
  if (items[0]?.kind !== "hot") problems.push(`the dispatch case does not start with the "${HOT_MARKER}" comment`);
  const coldIndex = items.findIndex((item) => item.kind === "cold");
  if (coldIndex < 0) problems.push(`the dispatch case has no "${COLD_MARKER}" comment`);
  const hot = items.slice(1, coldIndex < 0 ? items.length : coldIndex)
    .filter((item): item is Extract<Item, { kind: "arm" }> => item.kind === "arm")
    .map((item) => item.labels.join(", "));
  for (let index = 0; index < Math.max(hot.length, hotArms.length); index++) {
    if (hot[index] !== hotArms[index]) {
      problems.push(
        `hot arm ${index + 1} is ${hot[index] ?? "missing"}, expected ${hotArms[index] ?? "none"} ` +
          "(HOT_ARMS in scripts/check-vm-dispatch-layout.ts)",
      );
      break;
    }
  }
  const seen = new Map<string, number>();
  for (const item of items) {
    if (item.kind !== "arm") continue;
    for (const label of item.labels) {
      const first = seen.get(label);
      if (first !== undefined) problems.push(`${label} labels the arms at lines ${first} and ${item.line}`);
      else seen.set(label, item.line);
    }
  }
  return problems;
};

export const checkAlignment = (source: string): string[] => {
  const pattern =
    /\{\$IFDEF FPC\}\{\$PUSH\}\{\$CODEALIGN JUMP=64\}\{\$ENDIF\}\s*\nfunction TGocciaVM\.ExecuteClosureRegistersInternal\(/;
  const match = pattern.exec(source);
  if (!match) {
    return ["ExecuteClosureRegistersInternal is not preceded by {$IFDEF FPC}{$PUSH}{$CODEALIGN JUMP=64}{$ENDIF}"];
  }
  const body = source.slice(match.index);
  const end = /\nend;\n\{\$IFDEF FPC\}\{\$POP\}\{\$ENDIF\}\n/.exec(body);
  const next = /\n(?:function|procedure|constructor|destructor) /.exec(body.slice(match[0].length));
  if (!end || (next && next.index + match[0].length < end.index)) {
    return ["ExecuteClosureRegistersInternal is not followed by {$IFDEF FPC}{$POP}{$ENDIF}"];
  }
  return [];
};

// The case's arms by mean share of executed opcodes over the profiles, each a
// `{ opcodes: [{ opcode, count }] }` report, highest first.
export const rankArms = (
  source: string,
  profiles: Array<{ opcodes: Array<{ opcode: string; count: number }> }>,
): Array<{ arm: string; share: number }> => {
  const shares = new Map<string, number>();
  for (const profile of profiles) {
    const total = profile.opcodes.reduce((sum, entry) => sum + entry.count, 0);
    if (total === 0) continue;
    for (const entry of profile.opcodes) {
      shares.set(entry.opcode, (shares.get(entry.opcode) ?? 0) + entry.count / total / profiles.length);
    }
  }
  return caseItems(source)
    .filter((item): item is Extract<Item, { kind: "arm" }> => item.kind === "arm")
    .map((item) => ({
      arm: item.labels.join(", "),
      share: item.labels.reduce((sum, label) => sum + (shares.get(label) ?? 0), 0),
    }))
    .sort((left, right) => right.share - left.share);
};

const selfTest = (): void => {
  const hotArms = ["OP_A, OP_B", "OP_C"];
  const caseText = (body: string[]): string =>
    ["          case TGocciaOpCode(Op) of", ...body, "        else", "          // OP_CALL_SELF is dispatched here.", "          ;", "        end;"].join("\n");
  const good = [
    `      ${HOT_MARKER} Measured.`,
    "      OP_A,",
    "      OP_B:",
    "        X := 1;",
    "      OP_C:",
    "        X := 2;",
    `      ${COLD_MARKER} the rest.`,
    "      OP_D:",
    "        if X = 0 then",
    "          X := 3",
    "        else",
    "          X := 4;",
  ];
  const cases: Array<[string, string[], boolean]> = [
    ["the measured order", good, true],
    ["a cold arm before the hot arms", [good[0], "      OP_D:", "        X := 3;", ...good.slice(1, 6), good[6]], false],
    ["hot arms out of order", [good[0], ...good.slice(4, 6), ...good.slice(1, 4), ...good.slice(6)], false],
    ["no cold marker", good.filter((line) => !line.includes(COLD_MARKER)), false],
    ["no hot marker", good.slice(1), false],
    ["a label used twice", [...good, "      OP_C:", "        X := 4;"], false],
  ];
  let failed = 0;
  const ranked = rankArms(caseText(good), [
    { opcodes: [{ opcode: "OP_C", count: 6 }, { opcode: "OP_A", count: 1 }, { opcode: "OP_B", count: 3 }] },
    { opcodes: [{ opcode: "OP_D", count: 1 }, { opcode: "OP_C", count: 1 }] },
  ]);
  if (ranked.map((entry) => entry.arm).join("; ") !== "OP_C; OP_D; OP_A, OP_B" || Math.abs(ranked[0].share - 0.55) > 1e-9) {
    failed++;
    console.error(`self-test: ranking: got ${JSON.stringify(ranked)}`);
  }
  try {
    checkCase(caseText([...good, "      OP_E..OP_F:", "        X := 5;"]), hotArms);
    failed++;
    console.error("self-test: a label the check does not understand: expected an error");
  } catch {
    // expected
  }
  for (const [name, body, ok] of cases) {
    const problems = checkCase(caseText(body), hotArms);
    if ((problems.length === 0) !== ok) {
      failed++;
      console.error(`self-test: ${name}: expected ${ok ? "no problem" : "a problem"}, got ${JSON.stringify(problems)}`);
    }
  }
  const aligned = [
    "{$IFDEF FPC}{$PUSH}{$CODEALIGN JUMP=64}{$ENDIF}",
    "function TGocciaVM.ExecuteClosureRegistersInternal(",
    "begin",
    "end;",
    "{$IFDEF FPC}{$POP}{$ENDIF}",
    "",
  ].join("\n");
  const alignmentCases: Array<[string, string, boolean]> = [
    ["the aligned function", aligned, true],
    ["no alignment", aligned.replace("{$IFDEF FPC}{$PUSH}{$CODEALIGN JUMP=64}{$ENDIF}\n", ""), false],
    ["no pop", aligned.replace("{$IFDEF FPC}{$POP}{$ENDIF}\n", ""), false],
  ];
  for (const [name, text, ok] of alignmentCases) {
    if ((checkAlignment(text).length === 0) !== ok) {
      failed++;
      console.error(`self-test: ${name}: expected ${ok ? "no problem" : "a problem"}`);
    }
  }
  if (failed > 0) process.exit(1);
  console.log(`${cases.length + alignmentCases.length + 2} self-test case(s) passed.`);
};

const argValue = (name: string): string | undefined => {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : undefined;
};

if (process.argv.includes("--self-test")) {
  selfTest();
} else if (argValue("--rank") !== undefined) {
  const dir = argValue("--rank") as string;
  const threshold = Number(argValue("--threshold") ?? "0.001");
  const profiles = readdirSync(dir)
    .filter((name) => name.endsWith(".json"))
    .map((name) => JSON.parse(readFileSync(join(dir, name), "utf8")));
  if (profiles.length === 0) throw new Error(`no opcode profiles (*.json) in ${dir}`);
  const ranked = rankArms(readFileSync(DISPATCH_CASE, "utf8"), profiles).filter((entry) => entry.share >= threshold);
  console.log(`// ${ranked.length} arms with a mean share of at least ${threshold} over ${profiles.length} profiles`);
  for (const { arm, share } of ranked) console.log(`  "${arm}", // ${(share * 100).toFixed(2)}%`);
} else {
  const problems = [
    ...checkCase(readFileSync(DISPATCH_CASE, "utf8")),
    ...checkAlignment(readFileSync(VM_UNIT, "utf8")),
  ];
  if (problems.length > 0) {
    for (const problem of problems) console.error(problem);
    console.error(
      "\nThe dispatch layout is measured (issue #1917): keep the hot arms first, in " +
        "HOT_ARMS order, and the dispatch function's 64-byte arm alignment, or measure " +
        'the new layout as "Dispatch layout" in docs/bytecode-vm.md describes and update ' +
        "scripts/check-vm-dispatch-layout.ts with it.",
    );
    process.exit(1);
  }
  console.log(`Dispatch layout as measured: ${HOT_ARMS.length} hot arms first, arms 64-byte aligned.`);
}
