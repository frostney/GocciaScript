#!/usr/bin/env npx tsx
/**
 * check-trunc-guards.ts
 *
 * Rejects new `Trunc(<expr>.ToNumberLiteral...)` call sites in Pascal source.
 *
 * FPC's Trunc() raises a range-check error on NaN/±Infinity under -Cr (dev
 * builds) and silently produces garbage on aarch64, so script-controlled
 * numbers must never reach a bare Trunc. Route coercions through the helpers
 * in Goccia.Utils instead:
 *   - ToIntegerValue / ToIntegerFromArgs   (ToIntegerOrInfinity, saturating)
 *   - ToLengthValue                        (ToLength)
 *   - ToIntegerWithTruncationValue / ...64Value (RangeError on non-finite)
 *   - ToInt64Value                         (host/FFI marshaling, NaN/±∞ → 0)
 *   - ToInt32Value / ToUint32Value / ToUint16Value (modular wrapping)
 *
 * Allowlisted sites are pre-guarded or operate on engine-internal values;
 * adding to the allowlist requires demonstrating a non-finite guard in scope.
 *
 * Usage:
 *   npx tsx scripts/check-trunc-guards.ts
 *   npx tsx scripts/check-trunc-guards.ts --verbose
 */

import { readFileSync, readdirSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const SOURCE_DIRS = [
  join(ROOT, "source", "units"),
  join(ROOT, "source", "shared"),
  join(ROOT, "source", "app"),
];
const VERBOSE = process.argv.includes("--verbose");

// Bare Trunc on a freshly-coerced script value, including wrapped multi-line
// forms (the pattern is matched on the whole file with line tracking below).
const PATTERN = /Trunc\s*\(\s*[A-Za-z_][\w.()\s]*?\.ToNumberLiteral/g;

// file (relative to repo root) → number of allowed matches, with the reason
// the site is safe. Counts are exact so removals are noticed too.
const ALLOWLIST: Record<string, { count: number; reason: string }> = {
  "source/units/Goccia.Builtins.Console.pas": {
    count: 1,
    reason:
      "console.count counter map is engine-internal; values are always engine-created finite numbers",
  },
};

const walkPas = (dir: string, out: string[]): void => {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const full = join(dir, entry.name);
    if (entry.isDirectory()) walkPas(full, out);
    else if (entry.name.endsWith(".pas") || entry.name.endsWith(".dpr"))
      out.push(full);
  }
};

const files: string[] = [];
for (const dir of SOURCE_DIRS) walkPas(dir, files);

let failures = 0;
for (const file of files) {
  const rel = relative(ROOT, file);
  const text = readFileSync(file, "utf8");
  const matches = [...text.matchAll(PATTERN)];
  const allowed = ALLOWLIST[rel]?.count ?? 0;

  if (matches.length === allowed) {
    if (VERBOSE && matches.length > 0)
      console.log(`ok (allowlisted ×${allowed}): ${rel}`);
    continue;
  }

  failures++;
  console.error(
    `${rel}: ${matches.length} bare Trunc(...ToNumberLiteral...) site(s), ` +
      `${allowed} allowed`,
  );
  for (const m of matches) {
    const line = text.slice(0, m.index).split("\n").length;
    console.error(`  ${rel}:${line}: ${m[0].replace(/\s+/g, " ")}...`);
  }
}

if (failures > 0) {
  console.error(
    "\nBare Trunc on script-controlled numbers crashes dev builds on " +
      "NaN/±Infinity. Use the coercion helpers in Goccia.Utils (see header " +
      "of scripts/check-trunc-guards.ts), or extend the allowlist with a " +
      "documented guard.",
  );
  process.exit(1);
}

console.log(`${files.length} Pascal file(s) checked. No unguarded Trunc-on-ToNumberLiteral sites.`);
