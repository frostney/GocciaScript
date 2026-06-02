#!/usr/bin/env bun
/**
 * tc39-mcp-bump-pin.ts
 *
 * Update checked-in tc39-mcp npm version pins.
 * Invoked by the weekly tc39-mcp-bump.yml workflow.
 *
 * Usage:
 *   bun scripts/tc39-mcp-bump-pin.ts <semver-version> [target...]
 */

import { readFileSync, writeFileSync } from "fs";

const VERSION_RE =
  /^(?:0|[1-9]\d*)\.(?:0|[1-9]\d*)\.(?:0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?$/;
const PIN_RE = /tc39-mcp@((?:0|[1-9]\d*)\.(?:0|[1-9]\d*)\.(?:0|[1-9]\d*)(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?)/g;

const DEFAULT_TARGETS = [
  ".mcp.example.json",
  "AGENTS.md",
  ".agents/skills/gocciascript-issue-validation/SKILL.md",
];

function updateTarget(target: string, version: string): boolean {
  let text: string;
  try {
    text = readFileSync(target, "utf8");
  } catch (err) {
    console.error(`Failed to read ${target}: ${(err as Error).message}`);
    process.exitCode = 1;
    return false;
  }

  let seen = 0;
  let changed = 0;
  const updated = text.replace(PIN_RE, (full, oldVersion) => {
    seen++;
    if (oldVersion === version) return full;
    changed++;
    return `tc39-mcp@${version}`;
  });

  if (seen === 0) {
    console.error(`${target}: no tc39-mcp@<version> pin found`);
    process.exitCode = 1;
    return false;
  }

  if (changed > 0) {
    writeFileSync(target, updated);
    console.log(`${target}: bumped ${changed} pin(s) -> ${version}`);
    return true;
  }

  console.log(`${target}: already at ${version}`);
  return false;
}

function main(argv: string[]): number {
  if (argv.length < 1 || !VERSION_RE.test(argv[0])) {
    console.error(
      "Usage: bun scripts/tc39-mcp-bump-pin.ts <semver-version> [target...]",
    );
    return 2;
  }

  const [version, ...targets] = argv;
  const resolvedTargets = targets.length > 0 ? targets : DEFAULT_TARGETS;
  for (const target of resolvedTargets) {
    updateTarget(target, version);
  }

  return process.exitCode ?? 0;
}

process.exit(main(process.argv.slice(2)));
