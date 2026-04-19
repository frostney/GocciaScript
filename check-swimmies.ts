#!/usr/bin/env bun

// Checks files against rules defined in swimmies.json.
//
// Usage:
//   bun check-swimmies.ts file1 file2 ...          Check specific files
//   bun check-swimmies.ts --from-env                Read file_path from CLAUDE_TOOL_INPUT
//   bun check-swimmies.ts --block file1 file2 ...   Exit 1 on match (blocking mode)
//
// Default exit code is 0 (warning). Pass --block to exit 1 when rules match.

import { existsSync, readFileSync } from "fs";
import { resolve, relative, join } from "path";
import { execFileSync } from "child_process";
import { Glob } from "bun";

const ROOT = import.meta.dirname;

interface RawRule {
  glob: string | string[];
  files: string[];
  message?: string;
  newOnly?: boolean;
}

interface Rule {
  globs: string[];
  files: string[];
  message?: string;
  newOnly: boolean;
}

function existsInHead(filePath: string): boolean {
  try {
    execFileSync("git", ["cat-file", "-e", `HEAD:${filePath}`], {
      cwd: ROOT,
      stdio: "ignore",
    });
    return true;
  } catch {
    return false;
  }
}

function toRelative(filePath: string): string | null {
  const abs = resolve(filePath);
  const rel = relative(ROOT, abs);
  if (rel.startsWith("..")) return null;
  return rel;
}

function loadRules(): Rule[] {
  const configPath = join(ROOT, "swimmies.json");
  if (!existsSync(configPath)) return [];
  const raw: RawRule[] = JSON.parse(readFileSync(configPath, "utf8"));
  return raw.map((r) => ({
    globs: Array.isArray(r.glob) ? r.glob : [r.glob],
    files: r.files,
    message: r.message,
    newOnly: r.newOnly ?? false,
  }));
}

function matchesAnyGlob(filePath: string, patterns: string[]): boolean {
  for (const pattern of patterns) {
    if (new Glob(pattern).match(filePath)) return true;
  }
  return false;
}

function checkFiles(
  files: string[],
  block: boolean
): void {
  const rules = loadRules();
  if (rules.length === 0) return;

  const hits: { file: string; files: string[]; message?: string }[] = [];

  for (const file of files) {
    const rel = toRelative(file);
    if (!rel) continue;

    for (const rule of rules) {
      if (!matchesAnyGlob(rel, rule.globs)) continue;
      if (rule.newOnly && existsInHead(rel)) continue;
      hits.push({ file: rel, files: rule.files, message: rule.message });
    }
  }

  if (hits.length === 0) return;

  // Group by file to avoid duplicate headers
  const byFile = new Map<
    string,
    { files: Set<string>; messages: string[] }
  >();

  for (const h of hits) {
    let entry = byFile.get(h.file);
    if (!entry) {
      entry = { files: new Set(), messages: [] };
      byFile.set(h.file, entry);
    }
    for (const f of h.files) entry.files.add(f);
    if (h.message) entry.messages.push(h.message);
  }

  for (const [file, entry] of byFile) {
    console.log("");
    console.log(`\u26a0  ${file}`);
    if (entry.messages.length > 0) {
      for (const msg of entry.messages) {
        console.log(`   ${msg}`);
      }
    }
    console.log("   Read the following files before making any changes:");
    for (const f of entry.files) {
      console.log(`   - ${f}`);
    }
    console.log("");
    console.log("   Do not proceed until you have reviewed the referenced files.");
  }
  console.log("");

  if (block) process.exit(1);
}

// --- entry point ---

const args = process.argv.slice(2);
const block = args.includes("--block");
const fromEnv = args.includes("--from-env");
const fileArgs = args.filter((a) => !a.startsWith("--"));

if (fromEnv) {
  const input = process.env.CLAUDE_TOOL_INPUT;
  if (input) {
    try {
      const parsed = JSON.parse(input);
      if (parsed.file_path) checkFiles([parsed.file_path], block);
    } catch {
      // malformed input, skip silently
    }
  }
} else if (fileArgs.length > 0) {
  checkFiles(fileArgs, block);
}
