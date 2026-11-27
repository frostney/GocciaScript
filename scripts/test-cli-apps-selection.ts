#!/usr/bin/env bun
/**
 * test-cli-apps-selection.ts
 *
 * Checks section selection for scripts/test-cli-apps.ts without starting a
 * Goccia binary:
 *
 * - SectionRunner and parseSectionArgs (scripts/test-cli/sections.ts) run
 *   with inert section bodies: which sections run, that a --section text
 *   matching nothing fails the run also next to one that matched, that a
 *   failing section does not stop the next one, and which options are
 *   refused.
 * - test-cli-apps.ts itself runs from an empty directory, where no binary
 *   exists, with --list and with a --section that matches nothing. --list
 *   must print only unique names and exit 0. The unmatched run must print
 *   nothing to stdout and exit 1 with "No section matched". Top-level code
 *   that prints or throws, such as a check written outside section() that
 *   starts a binary, fails one of the two; top-level code that does neither
 *   is not detected.
 */

import { resolve } from "path";
import { clean, mkdtemp } from "./test-cli/tmpdir";
import { parseSectionArgs, SectionRunner, type SectionOptions } from "./test-cli/sections";

const failures: string[] = [];
const check = (label: string, ok: boolean, detail: string): void => {
  if (!ok) failures.push(`${label}: ${detail}`);
};

/** Runs sections named `names` with inert bodies; `throwing` names throw. */
async function simulate(options: SectionOptions, names: string[], throwing: string[] = []) {
  const out: string[] = [];
  const err: string[] = [];
  const ran: string[] = [];
  const runner = new SectionRunner(options, (line) => out.push(line), (line) => err.push(line));
  for (const name of names)
    await runner.section(name, () => {
      ran.push(name);
      if (throwing.includes(name)) throw new Error(`${name} broke`);
    });
  const exitCode = runner.finish("demo.ts");
  return { ran, out: out.join("\n"), err: err.join("\n"), exitCode };
}

const NAMES = ["Stdin policy: piped...", "Stdin policy: closed...", "Bundler: compile...", "Loader: JSON output..."];

{
  const all = await simulate({ filters: [], list: false }, NAMES);
  check("no --section", all.ran.length === 4 && all.exitCode === 0 && all.out.endsWith("All demo.ts tests passed."),
    JSON.stringify(all));

  const some = await simulate({ filters: ["STDIN POLICY", "bundler: COMPILE"], list: false }, NAMES);
  check("two matching texts", JSON.stringify(some.ran) === JSON.stringify(NAMES.slice(0, 3)) && some.exitCode === 0 &&
    some.out.endsWith("All 3 selected demo.ts sections passed; --section skipped 1."), JSON.stringify(some));

  const typo = await simulate({ filters: ["stdin policy", "typo-does-not-match"], list: false }, NAMES);
  check("a matching text and a typo", typo.ran.length === 2 && typo.exitCode === 1 &&
    typo.err.includes('No section matched "typo-does-not-match"') && !typo.err.includes('"stdin policy"'), JSON.stringify(typo));

  const broken = await simulate({ filters: [], list: false }, NAMES, [NAMES[1]]);
  check("a failing section", broken.ran.length === 4 && broken.exitCode === 1 &&
    broken.out.includes("  FAILED: Stdin policy: closed... broke") && broken.err.includes(`  - ${NAMES[1]}`), JSON.stringify(broken));

  const list = await simulate({ filters: [], list: true }, NAMES);
  check("--list", list.ran.length === 0 && list.exitCode === 0 && list.out === NAMES.join("\n"), JSON.stringify(list));
}

{
  const parsed = parseSectionArgs(["--section=a", "--section", "b", "--list"]);
  check("parse --section and --list", JSON.stringify(parsed) === JSON.stringify({ filters: ["a", "b"], list: true }),
    JSON.stringify(parsed));
  for (const args of [["--bogus"], ["extra"], ["--section="], ["--section"], ["--list=yes"]])
    check(`refuse ${JSON.stringify(args)}`, "error" in parseSectionArgs(args), JSON.stringify(parseSectionArgs(args)));
}

{
  const script = resolve(import.meta.dir, "test-cli-apps.ts");
  const noMatch = "no section is named this";
  const cwd = mkdtemp("goccia-apps-selection-");
  const run = (args: string[]) => {
    const proc = Bun.spawnSync([process.execPath, "run", script, ...args], { cwd, stdout: "pipe", stderr: "pipe" });
    return { exitCode: proc.exitCode, stdout: proc.stdout.toString(), stderr: proc.stderr.toString() };
  };
  try {
    const list = run(["--list"]);
    const names = list.stdout.split(/\r?\n/).filter((line) => line !== "");
    const duplicates = names.filter((name, index) => names.indexOf(name) !== index);
    check("test-cli-apps.ts --list", list.exitCode === 0 && list.stderr === "" && names.length > 0 && duplicates.length === 0,
      `exit ${list.exitCode}, duplicates ${JSON.stringify(duplicates)}:\n${list.stderr}`);

    const none = run([`--section=${noMatch}`]);
    check("test-cli-apps.ts with an unmatched --section", none.exitCode === 1 && none.stdout === "" &&
      none.stderr.includes(`No section matched "${noMatch}"`), `exit ${none.exitCode}:\n${none.stdout}${none.stderr}`);
  } finally {
    clean(cwd);
  }
}

if (failures.length > 0) {
  console.error(`${failures.length} selection check(s) failed:\n${failures.map((f) => `  - ${f}`).join("\n")}`);
  process.exit(1);
}
console.log("test-cli-apps.ts selection checks passed.");
