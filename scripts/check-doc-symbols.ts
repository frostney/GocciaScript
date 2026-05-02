#!/usr/bin/env npx tsx
/**
 * check-doc-symbols.ts
 *
 * Walks markdown files, extracts Pascal symbol references (unit names and
 * TGoccia-prefixed type names), and verifies they exist in the codebase.
 * Exits non-zero if any referenced symbol is not found.
 *
 * Scope:
 *   - Unit references: backtick-quoted `Goccia.Something.pas` or `Goccia.Something`
 *   - Type references: backtick-quoted `TGocciaXxx` identifiers (project types only)
 *
 * Not checked (by design):
 *   - FPC standard library types (TStringList, TDictionary, TObjectList, etc.)
 *   - JavaScript runtime names (TypeError, TypedArray, etc.)
 *   - Pascal keywords and primitives (True, TBytes, etc.)
 *   - Method/function names (ToStringLiteral, ThisValue, etc.)
 *   - Non-Goccia T-prefixed types (TBaseMap, THashMap, TTestSuite, etc.)
 *
 * Usage:
 *   npx tsx scripts/check-doc-symbols.ts
 *   npx tsx scripts/check-doc-symbols.ts --verbose
 */

import { readFileSync, readdirSync, existsSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";

// --- Configuration ---

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const SOURCE_DIRS = [
  join(ROOT, "source", "units"),
  join(ROOT, "source", "shared"),
  join(ROOT, "source", "app"),
];
const VERBOSE = process.argv.includes("--verbose");

const listPasFiles = (): { name: string; path: string }[] => {
  const results: { name: string; path: string }[] = [];
  for (const dir of SOURCE_DIRS) {
    if (!existsSync(dir)) continue;
    for (const file of readdirSync(dir)) {
      if (file.endsWith(".pas") || file.endsWith(".dpr")) {
        results.push({ name: file, path: join(dir, file) });
      }
    }
  }
  return results;
};

// Note: docs/spikes/ excluded — spike docs are snapshots per CONTRIBUTING.md
// and may reference historical type names that no longer exist.
const MD_DIRS = ["docs", "docs/contributing"];
const MD_ROOT_FILES = ["README.md", "CONTRIBUTING.md", "AGENTS.md"];

// Unit-reference patterns to skip (not real units)
const SKIP_UNIT_PATTERNS = [
  /^Goccia\.Values\.Your/,          // template placeholder
  /^Goccia\.Builtins\.Global(?:Your|FFI)/, // template placeholder
  /^Goccia\.inc$/,                   // include file
  /^Goccia\.build$/,                 // JSON config
  /^Goccia\.semver$/,                // JSON config
  /^Goccia\.gc(?:\.\w+)*$/,          // JavaScript API (Goccia.gc, Goccia.gc.maxBytes, etc.)
];

// Pattern for method-path references: `Goccia.Unit.Name.MethodName`
// These reference a method/function inside a unit, e.g. `Goccia.Arithmetic.pas`'s `IsActualZero`
const METHOD_PATH_PATTERN = /^(Goccia(?:\.\w+)+)\.([A-Z]\w+)$/;

// Files to skip entirely (snapshots / historical records)
const SKIP_FILES = new Set([
  "docs/decision-log.md",           // chronological record — may reference historical type names
]);

// --- Collect known symbols from the codebase ---

type PasFile = { name: string; path: string };

const collectUnitNames = (pasFiles: PasFile[]): Set<string> => {
  const units = new Set<string>();
  for (const { name } of pasFiles) {
    units.add(name);
    units.add(name.replace(/\.(?:pas|dpr)$/, ""));
  }
  return units;
};

const collectTGocciaTypes = (pasFiles: PasFile[]): Set<string> => {
  /**
   * Scans all .pas files for any identifier matching TGoccia* that appears
   * in a type-declaration context or as a class/record/enum definition.
   * Also picks up identifiers that appear after `: TGoccia` (field/param types)
   * and in `= class(TGocciaXxx)` inheritance.
   *
   * This is intentionally broad — we want to catch every TGoccia-prefixed
   * identifier the codebase actually defines or uses as a type.
   */
  const types = new Set<string>();
  for (const { path } of pasFiles) {
    const content = readFileSync(path, "utf-8");

    // Broad scan: any TGoccia-prefixed word boundary match
    const matches = content.matchAll(/\b(TGoccia\w+)\b/g);
    for (const m of matches) {
      types.add(m[1]);
    }
  }
  return types;
};

const collectMembers = (pasFiles: PasFile[]): Map<string, Set<string>> => {
  /**
   * Builds a map of ClassName -> Set<MemberName> from:
   *   - Method implementations: `function TGocciaXxx.Foo(...)`
   *   - Interface declarations: `function Foo(...); virtual;` inside a class block
   *   - Field declarations: `FFieldName: Type;`
   *   - Property declarations: `property PropName: Type`
   *
   * Uses a broad scan: any `TGocciaXxx.Identifier` in the source.
   */
  const members = new Map<string, Set<string>>();

  const addMember = (className: string, memberName: string) => {
    if (!members.has(className)) {
      members.set(className, new Set());
    }
    members.get(className)!.add(memberName);
  };

  for (const { path } of pasFiles) {
    const content = readFileSync(path, "utf-8");

    // Method implementations and class function/procedure references: TGocciaXxx.MemberName
    const dotMatches = content.matchAll(/\b(TGoccia\w+)\.(\w+)\b/g);
    for (const m of dotMatches) {
      addMember(m[1], m[2]);
    }

    // Interface-section declarations inside class blocks.
    // Parse class blocks and extract field/method/property names.
    const classBlockMatches = content.matchAll(/\b(TGoccia\w+)\s*=\s*class\b[^]*?(?=\bend\s*;)/g);
    for (const block of classBlockMatches) {
      const className = block[1];
      const body = block[0];

      // Fields: FFieldName: Type
      const fieldMatches = body.matchAll(/\b(F\w+)\s*:/g);
      for (const fm of fieldMatches) {
        addMember(className, fm[1]);
      }

      // Methods declared in interface: function/procedure MethodName
      const methodMatches = body.matchAll(/(?:function|procedure|constructor|destructor)\s+(\w+)\s*[\(;:]/g);
      for (const mm of methodMatches) {
        addMember(className, mm[1]);
      }

      // Properties: property PropName
      const propMatches = body.matchAll(/property\s+(\w+)\s*[;:]/g);
      for (const pm of propMatches) {
        addMember(className, pm[1]);
      }
    }
  }
  return members;
};

const collectUnitFunctions = (pasFiles: PasFile[]): Map<string, Set<string>> => {
  /**
   * Builds a map of UnitName -> Set<FunctionName> for free functions/procedures
   * and class method implementations within each unit.
   */
  const unitFns = new Map<string, Set<string>>();
  for (const { name, path } of pasFiles) {
    const unitName = name.replace(/\.(?:pas|dpr)$/, "");
    const content = readFileSync(path, "utf-8");
    const fns = new Set<string>();

    // Free functions/procedures
    const freeMatches = content.matchAll(/(?:^|\n)\s*(?:function|procedure)\s+(\w+)\s*[\(;:]/gm);
    for (const m of freeMatches) {
      fns.add(m[1]);
    }

    // Class methods: function TFoo.Bar
    const methodMatches = content.matchAll(/(?:function|procedure|constructor|destructor)\s+\w+\.(\w+)\s*[\(;:]/g);
    for (const m of methodMatches) {
      fns.add(m[1]);
    }

    unitFns.set(unitName, fns);
  }
  return unitFns;
};

// --- Extract symbol references from markdown ---

const collectMdFiles = (): string[] => {
  const files: string[] = [];

  for (const rootFile of MD_ROOT_FILES) {
    const fullPath = join(ROOT, rootFile);
    if (existsSync(fullPath)) {
      files.push(fullPath);
    }
  }

  for (const dir of MD_DIRS) {
    const fullDir = join(ROOT, dir);
    if (!existsSync(fullDir)) continue;
    for (const file of readdirSync(fullDir)) {
      if (file.endsWith(".md")) {
        files.push(join(fullDir, file));
      }
    }
  }

  return files;
};

interface SymbolRef {
  file: string;
  line: number;
  symbol: string;
  kind: "unit" | "type" | "method" | "method-path";
  className?: string;
  methodName?: string;
  unitName?: string;
  funcName?: string;
}

const extractSymbolRefs = (filePath: string): SymbolRef[] => {
  const content = readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  const refs: SymbolRef[] = [];
  const relPath = relative(ROOT, filePath);
  let inCodeBlock = false;

  for (const [i, line] of lines.entries()) {

    if (line.trimStart().startsWith("```")) {
      inCodeBlock = !inCodeBlock;
      continue;
    }
    if (inCodeBlock) continue;

    // Unit references: `Goccia.Something.pas` or `Goccia.Something`
    // Method-path references are resolved later in validation (need unitNames)
    const unitMatches = line.matchAll(/`(Goccia\.\w+(?:\.\w+)*(?:\.pas)?)`/g);
    for (const m of unitMatches) {
      refs.push({ file: relPath, line: i + 1, symbol: m[1], kind: "unit" });
    }

    // Method references: `TGocciaXxx.MethodName`
    const methodMatches = line.matchAll(/`(TGoccia\w+)\.(\w+)`/g);
    const methodSymbols = new Set<string>();
    for (const m of methodMatches) {
      const full = `${m[1]}.${m[2]}`;
      methodSymbols.add(full);
      refs.push({
        file: relPath, line: i + 1, symbol: full, kind: "method",
        className: m[1], methodName: m[2],
      });
    }

    // Type references: only `TGocciaXxx` (project-prefixed types, not already captured as method refs)
    const typeMatches = line.matchAll(/`(TGoccia\w+)`/g);
    for (const m of typeMatches) {
      // Skip if this was already captured as part of a method reference
      if ([...methodSymbols].some((ms) => ms.startsWith(`${m[1]}.`))) continue;
      refs.push({ file: relPath, line: i + 1, symbol: m[1], kind: "type" });
    }
  }

  return refs;
};

// --- Main ---

const main = () => {
  console.log("Checking documentation symbol references...\n");

  const pasFiles = listPasFiles();
  const unitNames = collectUnitNames(pasFiles);
  const typeNames = collectTGocciaTypes(pasFiles);
  const members = collectMembers(pasFiles);
  const unitFunctions = collectUnitFunctions(pasFiles);
  const mdFiles = collectMdFiles();

  if (VERBOSE) {
    console.log(`  Found ${unitNames.size} unit names, ${typeNames.size} TGoccia types, ${members.size} classes with members, ${unitFunctions.size} units with functions\n`);
  }

  let totalRefs = 0;
  let errors = 0;
  const errorMessages: string[] = [];

  for (const mdFile of mdFiles) {
    const relFile = relative(ROOT, mdFile);
    if (SKIP_FILES.has(relFile)) {
      if (VERBOSE) {
        console.log(`  SKIP  ${relFile} (snapshot/historical file)`);
      }
      continue;
    }

    const refs = extractSymbolRefs(mdFile);

    for (const ref of refs) {
      totalRefs++;

      // Skip known non-unit patterns
      if (ref.kind === "unit" && SKIP_UNIT_PATTERNS.some((p) => p.test(ref.symbol))) {
        if (VERBOSE) {
          console.log(`  SKIP  ${ref.file}:${ref.line} — ${ref.symbol}`);
        }
        continue;
      }

      let found = false;

      if (ref.kind === "unit") {
        const sym = ref.symbol;
        const withPas = sym.endsWith(".pas") ? sym : `${sym}.pas`;
        const withoutPas = sym.replace(/\.pas$/, "");

        if (sym.endsWith("*")) {
          // Wildcard: `Goccia.Evaluator.*`
          const prefix = sym.replace(/\.\*$/, ".");
          found = [...unitNames].some((u) => u.startsWith(prefix));
        } else if (unitNames.has(withPas) || unitNames.has(withoutPas)) {
          // Direct unit match
          found = true;
        } else if (!sym.endsWith(".pas")) {
          // Try as method-path: split last segment as function name
          const methodPathMatch = METHOD_PATH_PATTERN.exec(sym);
          if (methodPathMatch) {
            const unitPart = methodPathMatch[1];
            const funcPart = methodPathMatch[2];
            const unitFns = unitFunctions.get(unitPart);
            const unitExists = unitNames.has(unitPart) || unitNames.has(`${unitPart}.pas`);
            found = unitExists && unitFns !== undefined && unitFns.has(funcPart);
          }
        }
      } else if (ref.kind === "type") {
        found = typeNames.has(ref.symbol);
      } else if (ref.kind === "method") {
        const classMembers = members.get(ref.className!);
        found = classMembers !== undefined && classMembers.has(ref.methodName!);
      }

      if (found) {
        if (VERBOSE) {
          console.log(`  OK    ${ref.file}:${ref.line} — ${ref.symbol}`);
        }
      } else {
        errors++;
        const msg = `  FAIL  ${ref.file}:${ref.line} — ${ref.symbol} (${ref.kind} not found in codebase)`;
        errorMessages.push(msg);
        if (VERBOSE) {
          console.log(msg);
        }
      }
    }
  }

  console.log(`\nScanned ${mdFiles.length} markdown files, ${totalRefs} symbol references.`);

  if (errors > 0) {
    console.log(`\n${errors} broken reference(s):\n`);
    for (const msg of errorMessages) {
      console.log(msg);
    }
    console.log("");
    process.exit(1);
  } else {
    console.log("All symbol references are valid.");
  }
};

main();
