#!/usr/bin/env bun
/**
 * test-cli-apps-selection.ts
 *
 * Checks that scripts/test-cli-apps.ts runs nothing outside its section()
 * calls, which --section and --list rely on: a check written at the top
 * level would run whatever the selection. The script runs twice from an
 * empty directory, where no Goccia binary exists. `--list` must print only
 * unique section names, and a --section text that matches no section must
 * print nothing to stdout and fail with "No section matched". A top-level
 * check that starts a binary or prints a header fails one of the two.
 * Spawns no Goccia binary itself.
 */

import { mkdtempSync, rmSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";

const SCRIPT = resolve(import.meta.dir, "test-cli-apps.ts");
const NO_MATCH = "no section is named this";

const cwd = mkdtempSync(join(tmpdir(), "goccia-apps-selection-"));
const run = (args: string[]) => {
  const proc = Bun.spawnSync([process.execPath, "run", SCRIPT, ...args], {
    cwd,
    stdout: "pipe",
    stderr: "pipe",
  });
  return { exitCode: proc.exitCode, stdout: proc.stdout.toString(), stderr: proc.stderr.toString() };
};

try {
  const list = run(["--list"]);
  const names = list.stdout.split(/\r?\n/).filter((line) => line !== "");
  if (list.exitCode !== 0 || list.stderr !== "" || names.length === 0)
    throw new Error(`--list should print the section names and exit 0, got exit ${list.exitCode}:\n${list.stdout}${list.stderr}`);
  const duplicates = names.filter((name, index) => names.indexOf(name) !== index);
  if (duplicates.length > 0)
    throw new Error(`Section names should be unique, so --section can tell them apart: ${JSON.stringify(duplicates)}`);

  const none = run([`--section=${NO_MATCH}`]);
  if (none.exitCode !== 1 || none.stdout !== "" || !none.stderr.includes(`No section matched "${NO_MATCH}"`))
    throw new Error(`A --section that matches nothing should run nothing and exit 1, got exit ${none.exitCode}:\n${none.stdout}${none.stderr}`);

  console.log(`test-cli-apps.ts selection checks passed (${names.length} sections).`);
} finally {
  rmSync(cwd, { recursive: true, force: true });
}
