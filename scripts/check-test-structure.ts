#!/usr/bin/env bun
/**
 * Enforces the JavaScript test-suite layout rules documented in
 * docs/testing.md. The checks intentionally stay structural: they catch
 * reliable signs of catch-all files, misplaced prototype-method suites, and
 * parser rejection assertions hidden behind dynamic code generation.
 *
 * Usage:
 *   bun run scripts/check-test-structure.ts
 *   bun run scripts/check-test-structure.ts --verbose
 */

import { readFileSync, readdirSync } from "node:fs";
import { basename, dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";

const SCRIPT_DIRECTORY = dirname(fileURLToPath(import.meta.url));
const ROOT = join(SCRIPT_DIRECTORY, "..");
const TEST_ROOT = join(ROOT, "tests");
const VERBOSE = process.argv.includes("--verbose");

type AllowlistEntry = {
  reason: string;
};

type Finding = {
  file: string;
  line?: number;
  message: string;
};

const CATCH_ALL_NAME_PATTERN =
  /^(?:edge-cases|error-cases|methods|misc|prototype-methods|removed-methods|static-methods)\.js$/;

// These names describe multiple independently testable built-in operations.
// Keep the paths here after the files are split so recreating one fails CI.
const PROHIBITED_COMBINED_FILES = new Map<string, string>([
  [
    "tests/built-ins/Atomics/load-store-compare-exchange.js",
    "split Atomics.load, store, and compareExchange into their own files",
  ],
  [
    "tests/built-ins/Atomics/read-modify-write.js",
    "split each Atomics read-modify-write method into its own file",
  ],
  [
    "tests/built-ins/Atomics/wait-notify.js",
    "split Atomics.wait and Atomics.notify into their own files",
  ],
  [
    "tests/built-ins/BigInt/prototype-methods.js",
    "put each BigInt prototype method under BigInt/prototype/",
  ],
  [
    "tests/built-ins/Date/methods.js",
    "split Date static and prototype methods by operation",
  ],
  [
    "tests/built-ins/Intl/Locale/locale-info-methods.js",
    "split each Intl.Locale information method into its own file",
  ],
  [
    "tests/built-ins/Math/hyperbolic.js",
    "split each Math hyperbolic method into its own file",
  ],
  [
    "tests/built-ins/Math/logarithmic.js",
    "split each Math logarithmic method into its own file",
  ],
  [
    "tests/built-ins/Math/misc.js",
    "split each Math method into its own file",
  ],
  [
    "tests/built-ins/Math/rounding.js",
    "split each Math rounding method into its own file",
  ],
  [
    "tests/built-ins/Promise/error-cases.js",
    "co-locate each failure case with the Promise operation it exercises",
  ],
  [
    "tests/built-ins/Symbol/symbol.js",
    "split Symbol constructor, registry, and prototype behavior by operation",
  ],
  [
    "tests/built-ins/constructors/require-new.js",
    "co-locate the new-required contract with each constructor",
  ],
]);

// These files intentionally span methods because the shared cross-surface
// invariant is itself the behavior under test. Entries are exact and must
// remain present; stale entries fail the check.
const CROSS_SURFACE_CONTRACT_ALLOWLIST = new Map<string, AllowlistEntry>([
  [
    "tests/built-ins/Temporal/removed-methods.js",
    {
      reason:
        "one proposal-version absence contract spanning methods removed together",
    },
  ],
  [
    "tests/built-ins/Error/error-prototype-constructor.js",
    {
      reason:
        "one constructor-family invariant across Error and NativeError prototypes",
    },
  ],
  [
    "tests/built-ins/Date/prototype/receiver-branding.js",
    {
      reason:
        "one shared Date internal-slot receiver contract across registered prototype methods",
    },
  ],
  [
    "tests/built-ins/Intl/Locale/prototype/receiver-branding.js",
    {
      reason:
        "one shared Intl.Locale internal-slot receiver contract across information methods",
    },
  ],
  [
    "tests/built-ins/WeakMap/prototype/absent-enumeration-apis.js",
    {
      reason:
        "one weak-collection non-enumerability contract spanning APIs absent together",
    },
  ],
  [
    "tests/built-ins/WeakSet/prototype/absent-enumeration-apis.js",
    {
      reason:
        "one weak-collection non-enumerability contract spanning APIs absent together",
    },
  ],
]);

// Parser failures are part of the Function constructor contract only in this
// directory. Other malformed source belongs in the CLI parser/lexer suites.
const DYNAMIC_REJECTION_ALLOWLIST = [
  {
    prefix: "tests/built-ins/Function/",
    reason: "the dynamic-code API itself is the operation under test",
  },
];

const toRepositoryPath = (path: string): string =>
  relative(ROOT, path).split("\\").join("/");

const escapeRegularExpression = (value: string): string =>
  value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

const walkJavaScript = (directory: string, output: string[]): void => {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) {
      walkJavaScript(path, output);
    } else if (entry.name.endsWith(".js")) {
      output.push(path);
    }
  }
};

const lineAt = (text: string, index: number): number =>
  text.slice(0, index).split("\n").length;

const findClosingParenthesis = (
  text: string,
  openingIndex: number,
): number | undefined => {
  let depth = 0;
  let quote: "'" | '"' | "`" | undefined;
  let lineComment = false;
  let blockComment = false;

  for (let index = openingIndex; index < text.length; index++) {
    const character = text[index];
    const next = text[index + 1];

    if (lineComment) {
      if (character === "\n") lineComment = false;
      continue;
    }
    if (blockComment) {
      if (character === "*" && next === "/") {
        blockComment = false;
        index++;
      }
      continue;
    }
    if (quote) {
      if (character === "\\") {
        index++;
      } else if (character === quote) {
        quote = undefined;
      }
      continue;
    }
    if (character === "/" && next === "/") {
      lineComment = true;
      index++;
      continue;
    }
    if (character === "/" && next === "*") {
      blockComment = true;
      index++;
      continue;
    }
    if (character === "'" || character === '"' || character === "`") {
      quote = character;
      continue;
    }
    if (character === "(") {
      depth++;
    } else if (character === ")") {
      depth--;
      if (depth === 0) return index;
    }
  }
  return undefined;
};

const findDynamicCodeCall = (text: string): number | undefined => {
  let quote: "'" | '"' | "`" | undefined;
  let lineComment = false;
  let blockComment = false;

  for (let index = 0; index < text.length; index++) {
    const character = text[index];
    const next = text[index + 1];

    if (lineComment) {
      if (character === "\n") lineComment = false;
      continue;
    }
    if (blockComment) {
      if (character === "*" && next === "/") {
        blockComment = false;
        index++;
      }
      continue;
    }
    if (quote) {
      if (character === "\\") {
        index++;
      } else if (character === quote) {
        quote = undefined;
      }
      continue;
    }
    if (character === "/" && next === "/") {
      lineComment = true;
      index++;
      continue;
    }
    if (character === "/" && next === "*") {
      blockComment = true;
      index++;
      continue;
    }
    if (character === "'" || character === '"' || character === "`") {
      quote = character;
      continue;
    }

    for (const name of ["Function", "eval"]) {
      if (!text.startsWith(name, index)) continue;
      const previous = text[index - 1];
      const afterName = text[index + name.length];
      if (
        (previous && /[\w$]/.test(previous)) ||
        (afterName && /[\w$]/.test(afterName))
      )
        continue;

      let parenthesisIndex = index + name.length;
      while (/\s/.test(text[parenthesisIndex] ?? ""))
        parenthesisIndex++;
      if (text[parenthesisIndex] === "(") return index;
    }
  }

  return undefined;
};

const dynamicRejectionLocations = (text: string): number[] => {
  const locations: number[] = [];
  const expectPattern = /\bexpect\s*\(/g;

  for (const match of text.matchAll(expectPattern)) {
    const openingIndex = (match.index ?? 0) + match[0].lastIndexOf("(");
    const closingIndex = findClosingParenthesis(text, openingIndex);
    if (closingIndex === undefined) continue;

    const matcher = text.slice(closingIndex + 1, closingIndex + 100);
    if (!/^\s*(?:\.rejects)?\.toThrow(?:Error)?\s*\(/.test(matcher))
      continue;

    const expectation = text.slice(openingIndex + 1, closingIndex);
    const dynamicCall = findDynamicCodeCall(expectation);
    if (dynamicCall !== undefined)
      locations.push(openingIndex + 1 + dynamicCall);
  }

  return locations;
};

const builtInNamesForFile = (file: string): string[] => {
  const prefix = "tests/built-ins/";
  if (!file.startsWith(prefix)) return [];

  const segments = file.slice(prefix.length).split("/");
  segments.pop();
  const prototypeIndex = segments.indexOf("prototype");
  const ownerSegments =
    prototypeIndex === -1 ? segments : segments.slice(0, prototypeIndex);

  return ownerSegments.map((_, index) =>
    ownerSegments.slice(0, index + 1).join("."),
  );
};

const findMisplacedPrototypeContract = (
  text: string,
  file: string,
): number | undefined => {
  const operationPatterns = builtInNamesForFile(file).map((name) => {
    const escapedName = escapeRegularExpression(name);
    return new RegExp(
      `\\b${escapedName}\\.prototype(?:` +
        `\\.[A-Za-z_$][\\w$]*|` +
        `\\[Symbol\\.[A-Za-z_$][\\w$]*\\]|` +
        `\\s+(?:has\\b|methods?\\b)` +
        `)`,
    );
  });
  if (operationPatterns.length === 0) return undefined;

  const titleAt = (openingIndex: number): string | undefined => {
    let index = openingIndex + 1;
    while (/\s/.test(text[index] ?? "")) index++;
    const quote = text[index];
    if (quote !== "'" && quote !== '"' && quote !== "`") return undefined;

    let title = "";
    for (index++; index < text.length; index++) {
      const character = text[index];
      if (character === "\\") {
        title += character + (text[index + 1] ?? "");
        index++;
      } else if (character === quote) {
        return title;
      } else if (character === "\n") {
        return undefined;
      } else {
        title += character;
      }
    }
    return undefined;
  };

  const locations: number[] = [];
  const declarationPattern = /\b(?:describe|test|it)(?:\.\w+)*/g;
  for (const match of text.matchAll(declarationPattern)) {
    let openingIndex = (match.index ?? 0) + match[0].length;
    while (/\s/.test(text[openingIndex] ?? "")) openingIndex++;
    if (text[openingIndex] !== "(") continue;

    let title = titleAt(openingIndex);
    if (title === undefined) {
      const closingIndex = findClosingParenthesis(text, openingIndex);
      if (closingIndex === undefined) continue;
      openingIndex = closingIndex + 1;
      while (/\s/.test(text[openingIndex] ?? "")) openingIndex++;
      if (text[openingIndex] !== "(") continue;
      title = titleAt(openingIndex);
    }

    if (
      title !== undefined &&
      operationPatterns.some((pattern) => pattern.test(title))
    )
      locations.push(match.index ?? 0);
  }

  const metadataPattern = /^description:\s*([^\r\n]*)/gm;
  for (const match of text.matchAll(metadataPattern)) {
    if (operationPatterns.some((pattern) => pattern.test(match[1])))
      locations.push(match.index ?? 0);
  }

  return locations.length > 0 ? Math.min(...locations) : undefined;
};

const files: string[] = [];
walkJavaScript(TEST_ROOT, files);
files.sort();

const findings: Finding[] = [];
let allowlistedChecks = 0;
const presentCrossSurfaceContracts = new Set<string>();

for (const path of files) {
  const file = toRepositoryPath(path);
  const text = readFileSync(path, "utf8");
  const crossSurfaceContract = CROSS_SURFACE_CONTRACT_ALLOWLIST.get(file);

  if (crossSurfaceContract) {
    presentCrossSurfaceContracts.add(file);
    allowlistedChecks++;
    if (VERBOSE)
      console.log(
        `allowlisted cross-surface contract: ${file} (${crossSurfaceContract.reason})`,
      );
  }

  if (CATCH_ALL_NAME_PATTERN.test(basename(path))) {
    if (!crossSurfaceContract) {
      findings.push({
        file,
        message:
          "catch-all filename; use one operation per file and co-locate its edge cases",
      });
    }
  }

  const prohibitedReason = PROHIBITED_COMBINED_FILES.get(file);
  if (prohibitedReason && !CATCH_ALL_NAME_PATTERN.test(basename(path))) {
    findings.push({ file, message: prohibitedReason });
  }

  if (
    file.startsWith("tests/built-ins/") &&
    !file.includes("/prototype/")
  ) {
    const declarationIndex = findMisplacedPrototypeContract(text, file);
    if (declarationIndex !== undefined) {
      if (!crossSurfaceContract) {
        findings.push({
          file,
          line: lineAt(text, declarationIndex),
          message:
            "prototype member contract must live in the built-in's prototype/ directory",
        });
      }
    }
  }

  const dynamicAllowlist = DYNAMIC_REJECTION_ALLOWLIST.find(({ prefix }) =>
    file.startsWith(prefix),
  );
  const rejectionLocations = dynamicRejectionLocations(text);
  if (dynamicAllowlist) {
    if (rejectionLocations.length > 0) {
      allowlistedChecks++;
      if (VERBOSE)
        console.log(
          `allowlisted dynamic rejection: ${file} (${dynamicAllowlist.reason})`,
        );
    }
  } else {
    for (const location of rejectionLocations) {
      findings.push({
        file,
        line: lineAt(text, location),
        message:
          "parser rejection via Function/eval belongs in scripts/test-cli-parser.ts or test-cli-lexer.ts",
      });
    }
  }
}

for (const file of CROSS_SURFACE_CONTRACT_ALLOWLIST.keys()) {
  if (!presentCrossSurfaceContracts.has(file)) {
    findings.push({
      file,
      message:
        "stale cross-surface allowlist entry; remove it or restore the documented contract",
    });
  }
}

if (findings.length > 0) {
  console.error("JavaScript test-suite structure violations:\n");
  for (const finding of findings) {
    const location = finding.line
      ? `${finding.file}:${finding.line}`
      : finding.file;
    console.error(`  ${location}: ${finding.message}`);
  }
  console.error(
    "\nSee docs/testing.md. Add an allowlist entry only for a narrowly " +
      "justified single cross-surface contract, with its reason.",
  );
  process.exit(1);
}

console.log(
  `${files.length} JavaScript test file(s) checked; ` +
    `${allowlistedChecks} justified structural exception(s).`,
);
