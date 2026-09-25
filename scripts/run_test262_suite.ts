#!/usr/bin/env bun
/**
 * Merge native GocciaTest262Runner shard reports and render PR comments.
 *
 * Test discovery, execution, and classification belong to the native runner.
 * This script intentionally owns only artifact-level work that CI performs
 * after every shard has completed.
 *
 * Usage:
 *   bun scripts/run_test262_suite.ts --merge-shards [options] <shard.json>...
 *   bun scripts/run_test262_suite.ts --comment <results.json> <baseline.json|->
 *
 * See docs/test262.md for the report and sharding contract.
 */

import { readFileSync, existsSync, mkdirSync, writeFileSync } from "fs";
import { join, dirname, relative, resolve } from "path";

const PROFILE_REPORT_LIMIT = 50;

type Outcome = "PASS" | "FAIL" | "WRAPPER_INFRA" | "TIMEOUT";

interface PerTestRecord {
  id: string;
  status: Outcome;
  durationMs: number;
  message: string;
  diagnostic?: string;
  profilePath?: string;
  profileMissing?: boolean;
}

function profilePathForTest(profileDir: string, testId: string): string {
  return join(profileDir, "per-test", ...testId.replace(/\\/g, "/").split("/")) +
    ".profile.json";
}

// ---------------------------------------------------------------------------
// Aggregation
// ---------------------------------------------------------------------------

interface CategorySummary {
  category: string;
  run: number;
  passed: number;
  failed: number;
  wrapperInfra: number;
  timeouts: number;
}

interface SuiteSummary {
  totalDiscovered: number;
  totalRun: number;
  passed: number;
  failed: number;
  wrapperInfraFailures: number;
  timeouts: number;
  durationSeconds: number;
  byCategory: CategorySummary[];
}

interface SuiteRunMetadata {
  suiteDir: string;
  test262Sha: string | null;
  mode: "interpreted" | "bytecode";
  categories: string[];
  jobs: number;
  timeoutMs: number;
  maxMemoryBytes: number;
}

interface ShardMetadata {
  index: number;
  count: number;
}

interface SuiteReport {
  summary: SuiteSummary;
  results: PerTestRecord[];
  run?: SuiteRunMetadata;
  shard?: ShardMetadata;
}

function aggregate(
  results: PerTestRecord[],
  durationSeconds: number,
  totalDiscovered: number,
): SuiteSummary {
  const byCat = new Map<string, CategorySummary>();
  let passed = 0;
  let failed = 0;
  let wrapperInfra = 0;
  let timeouts = 0;
  for (const r of results) {
    const cat = r.id.split("/")[0] || "unknown";
    let entry = byCat.get(cat);
    if (!entry) {
      entry = {
        category: cat,
        run: 0,
        passed: 0,
        failed: 0,
        wrapperInfra: 0,
        timeouts: 0,
      };
      byCat.set(cat, entry);
    }
    entry.run++;
    switch (r.status) {
      case "PASS":
        entry.passed++;
        passed++;
        break;
      case "FAIL":
        entry.failed++;
        failed++;
        break;
      case "WRAPPER_INFRA":
        entry.wrapperInfra++;
        wrapperInfra++;
        break;
      case "TIMEOUT":
        entry.timeouts++;
        timeouts++;
        break;
    }
  }
  const sorted = Array.from(byCat.values()).sort((a, b) =>
    a.category.localeCompare(b.category),
  );
  return {
    totalDiscovered,
    totalRun: results.length,
    passed,
    failed,
    wrapperInfraFailures: wrapperInfra,
    timeouts,
    durationSeconds,
    byCategory: sorted,
  };
}

function shardForTest(testId: string, shardCount: number): number {
  // FNV-1a gives a stable, platform-independent distribution without coupling
  // shard membership to discovery order or test262 directory insertions.
  let hash = 0x811c9dc5;
  for (let index = 0; index < testId.length; index++) {
    hash ^= testId.charCodeAt(index);
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0) % shardCount;
}

// ---------------------------------------------------------------------------
// Profile aggregation
// ---------------------------------------------------------------------------

type ProfileJson = {
  opcodes?: Array<{ opcode?: string; count?: number }>;
  opcodePairs?: Array<{ prev?: string; cur?: string; count?: number }>;
  scalarFastPath?: { hits?: number; misses?: number; total?: number; hitRate?: number };
  shapeSaturation?: { depthLimitPrefixes?: number; tableCapacityEvents?: number };
  functions?: Array<{
    name?: string;
    sourceFile?: string;
    line?: number;
    calls?: number;
    selfTimeNs?: number;
    totalTimeNs?: number;
    allocations?: number;
  }>;
};

type CountEntry = { name: string; count: number; percentage?: number };
type FunctionEntry = {
  name: string;
  sourceFile: string;
  line: number;
  calls: number;
  selfTimeNs: number;
  totalTimeNs: number;
  allocations: number;
};
type TestProfileEntry = {
  id: string;
  status: Outcome;
  durationMs: number;
  profilePath: string;
  totalOpcodes: number;
  allocations: number;
  functionSelfTimeNs: number;
};
type ProfileGroupEntry = {
  key: string;
  profiles: number;
  totalOpcodes: number;
  allocations: number;
  functionSelfTimeNs: number;
};

type Test262ProfileSummary = {
  generatedAt: string;
  run: {
    suiteDir: string;
    test262Sha: string | null;
    mode: "bytecode";
    categories: string[];
    jobs: number;
    timeoutMs: number;
    maxMemoryBytes: number;
    totalDiscovered: number;
    totalRun: number;
    passed: number;
    failed: number;
    wrapperInfraFailures: number;
    timeouts: number;
    durationSeconds: number;
  };
  profileDir: string;
  profileCount: number;
  missingProfileCount: number;
  totalOpcodes: number;
  totalOpcodePairs: number;
  scalarFastPath: {
    hits: number;
    misses: number;
    total: number;
    hitRate: number | null;
  };
  shapeSaturation: {
    depthLimitPrefixes: number;
    tableCapacityEvents: number;
  };
  topOpcodes: CountEntry[];
  topOpcodePairs: CountEntry[];
  topFunctionsBySelfTime: FunctionEntry[];
  topFunctionsByCalls: FunctionEntry[];
  topFunctionsByAllocations: FunctionEntry[];
  topTestsByOpcodes: TestProfileEntry[];
  topTestsByAllocations: TestProfileEntry[];
  topTestsByDuration: TestProfileEntry[];
  categoryBreakdown: ProfileGroupEntry[];
  pathBreakdown: ProfileGroupEntry[];
};

function safeNumber(value: unknown): number {
  return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function addCount(map: Map<string, number>, key: string, count: number): void {
  if (!key || count <= 0) return;
  map.set(key, (map.get(key) ?? 0) + count);
}

function topCounts(map: Map<string, number>, total: number): CountEntry[] {
  return [...map.entries()]
    .map(([name, count]) => ({
      name,
      count,
      percentage: total > 0 ? (count / total) * 100 : undefined,
    }))
    .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name))
    .slice(0, PROFILE_REPORT_LIMIT);
}

function totalOpcodeCount(profile: ProfileJson): number {
  return (profile.opcodes ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.count),
    0,
  );
}

function totalAllocations(profile: ProfileJson): number {
  return (profile.functions ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.allocations),
    0,
  );
}

function totalFunctionSelfTime(profile: ProfileJson): number {
  return (profile.functions ?? []).reduce(
    (sum, entry) => sum + safeNumber(entry.selfTimeNs),
    0,
  );
}

function addGroup(
  map: Map<string, ProfileGroupEntry>,
  key: string,
  profile: ProfileJson,
): void {
  const entry =
    map.get(key) ??
    ({
      key,
      profiles: 0,
      totalOpcodes: 0,
      allocations: 0,
      functionSelfTimeNs: 0,
    } satisfies ProfileGroupEntry);
  entry.profiles++;
  entry.totalOpcodes += totalOpcodeCount(profile);
  entry.allocations += totalAllocations(profile);
  entry.functionSelfTimeNs += totalFunctionSelfTime(profile);
  map.set(key, entry);
}

function functionKey(entry: NonNullable<ProfileJson["functions"]>[number]): string {
  return [
    entry.name || "<anonymous>",
    entry.sourceFile || "",
    safeNumber(entry.line),
  ].join("\u0000");
}

function addFunction(
  map: Map<string, FunctionEntry>,
  entry: NonNullable<ProfileJson["functions"]>[number],
): void {
  const key = functionKey(entry);
  const current =
    map.get(key) ??
    ({
      name: entry.name || "<anonymous>",
      sourceFile: entry.sourceFile || "",
      line: safeNumber(entry.line),
      calls: 0,
      selfTimeNs: 0,
      totalTimeNs: 0,
      allocations: 0,
    } satisfies FunctionEntry);
  current.calls += safeNumber(entry.calls);
  current.selfTimeNs += safeNumber(entry.selfTimeNs);
  current.totalTimeNs += safeNumber(entry.totalTimeNs);
  current.allocations += safeNumber(entry.allocations);
  map.set(key, current);
}

function topFunctions(
  map: Map<string, FunctionEntry>,
  metric: keyof Pick<FunctionEntry, "selfTimeNs" | "calls" | "allocations">,
): FunctionEntry[] {
  return [...map.values()]
    .sort(
      (a, b) =>
        b[metric] - a[metric] ||
        b.calls - a.calls ||
        a.name.localeCompare(b.name) ||
        a.sourceFile.localeCompare(b.sourceFile) ||
        a.line - b.line,
    )
    .slice(0, PROFILE_REPORT_LIMIT);
}

function topTests(
  tests: TestProfileEntry[],
  metric: keyof Pick<TestProfileEntry, "totalOpcodes" | "allocations" | "durationMs">,
): TestProfileEntry[] {
  return [...tests]
    .sort((a, b) => b[metric] - a[metric] || a.id.localeCompare(b.id))
    .slice(0, PROFILE_REPORT_LIMIT);
}

function topGroups(map: Map<string, ProfileGroupEntry>): ProfileGroupEntry[] {
  return [...map.values()]
    .sort(
      (a, b) =>
        b.totalOpcodes - a.totalOpcodes ||
        b.allocations - a.allocations ||
        a.key.localeCompare(b.key),
    )
    .slice(0, PROFILE_REPORT_LIMIT);
}

function profilePathForDisplay(profileDir: string, profilePath: string): string {
  return relative(profileDir, profilePath).split("\\").join("/");
}

function buildProfileSummary(
  results: PerTestRecord[],
  profileDir: string,
  run: Test262ProfileSummary["run"],
): Test262ProfileSummary {
  const opcodeCounts = new Map<string, number>();
  const opcodePairCounts = new Map<string, number>();
  const functionTotals = new Map<string, FunctionEntry>();
  const categories = new Map<string, ProfileGroupEntry>();
  const paths = new Map<string, ProfileGroupEntry>();
  const tests: TestProfileEntry[] = [];
  let missingProfileCount = 0;
  let scalarHits = 0;
  let scalarMisses = 0;
  let depthLimitPrefixes = 0;
  let tableCapacityEvents = 0;
  let totalOpcodes = 0;
  let totalOpcodePairs = 0;

  for (const result of results) {
    const profilePath = profilePathForTest(profileDir, result.id);
    if (!existsSync(profilePath)) {
      missingProfileCount++;
      continue;
    }
    let profile: ProfileJson;
    try {
      profile = JSON.parse(readFileSync(profilePath, "utf8")) as ProfileJson;
    } catch {
      missingProfileCount++;
      continue;
    }

    const profileOpcodes = totalOpcodeCount(profile);
    const allocations = totalAllocations(profile);
    const functionSelfTimeNs = totalFunctionSelfTime(profile);
    totalOpcodes += profileOpcodes;
    scalarHits += safeNumber(profile.scalarFastPath?.hits);
    scalarMisses += safeNumber(profile.scalarFastPath?.misses);
    depthLimitPrefixes += safeNumber(profile.shapeSaturation?.depthLimitPrefixes);
    tableCapacityEvents += safeNumber(profile.shapeSaturation?.tableCapacityEvents);

    for (const entry of profile.opcodes ?? []) {
      addCount(opcodeCounts, entry.opcode || "", safeNumber(entry.count));
    }
    for (const entry of profile.opcodePairs ?? []) {
      const prev = entry.prev || "";
      const cur = entry.cur || "";
      const count = safeNumber(entry.count);
      if (prev && cur) addCount(opcodePairCounts, `${prev} -> ${cur}`, count);
      totalOpcodePairs += count;
    }
    for (const entry of profile.functions ?? []) {
      addFunction(functionTotals, entry);
    }

    const parts = result.id.split("/");
    addGroup(categories, parts[0] || "unknown", profile);
    addGroup(paths, parts.slice(0, 2).join("/") || "unknown", profile);
    tests.push({
      id: result.id,
      status: result.status,
      durationMs: result.durationMs,
      profilePath: profilePathForDisplay(profileDir, profilePath),
      totalOpcodes: profileOpcodes,
      allocations,
      functionSelfTimeNs,
    });
  }

  const scalarTotal = scalarHits + scalarMisses;
  return {
    generatedAt: new Date().toISOString(),
    run,
    profileDir,
    profileCount: tests.length,
    missingProfileCount,
    totalOpcodes,
    totalOpcodePairs,
    scalarFastPath: {
      hits: scalarHits,
      misses: scalarMisses,
      total: scalarTotal,
      hitRate: scalarTotal > 0 ? scalarHits / scalarTotal : null,
    },
    shapeSaturation: {
      depthLimitPrefixes,
      tableCapacityEvents,
    },
    topOpcodes: topCounts(opcodeCounts, totalOpcodes),
    topOpcodePairs: topCounts(opcodePairCounts, totalOpcodePairs),
    topFunctionsBySelfTime: topFunctions(functionTotals, "selfTimeNs"),
    topFunctionsByCalls: topFunctions(functionTotals, "calls"),
    topFunctionsByAllocations: topFunctions(functionTotals, "allocations"),
    topTestsByOpcodes: topTests(tests, "totalOpcodes"),
    topTestsByAllocations: topTests(tests, "allocations"),
    topTestsByDuration: topTests(tests, "durationMs"),
    categoryBreakdown: topGroups(categories),
    pathBreakdown: topGroups(paths),
  };
}

function formatProfileInteger(value: number): string {
  return Math.round(value).toLocaleString("en-US");
}

function formatNanoseconds(ns: number): string {
  const ms = ns / 1e6;
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`;
  return `${ms.toFixed(1)}ms`;
}

function writeProfileMarkdown(summary: Test262ProfileSummary, outputPath: string): void {
  const lines: string[] = [];
  lines.push("# test262 Profile Report", "");
  lines.push(`Generated: ${summary.generatedAt}`);
  lines.push(`Mode: ${summary.run.mode}`);
  lines.push(`test262 SHA: ${summary.run.test262Sha ?? "unknown"}`);
  lines.push(`Categories: ${summary.run.categories.join(", ")}`);
  lines.push(
    `Run settings: ${summary.run.jobs} jobs, ` +
      `${summary.run.timeoutMs}ms timeout, ` +
      `${formatProfileInteger(summary.run.maxMemoryBytes)} byte heap cap`,
  );
  lines.push(
    `Conformance outcomes: ${formatProfileInteger(summary.run.passed)} passed, ` +
      `${formatProfileInteger(summary.run.failed)} failed, ` +
      `${formatProfileInteger(summary.run.wrapperInfraFailures)} wrapper-infra, ` +
      `${formatProfileInteger(summary.run.timeouts)} timeouts`,
  );
  lines.push(`Profiles: ${formatProfileInteger(summary.profileCount)}`);
  if (summary.missingProfileCount > 0) {
    lines.push(`Missing/corrupt profiles: ${formatProfileInteger(summary.missingProfileCount)}`);
  }
  lines.push(`Total opcodes: ${formatProfileInteger(summary.totalOpcodes)}`);
  lines.push(`Total opcode pairs: ${formatProfileInteger(summary.totalOpcodePairs)}`);
  const hitRate = summary.scalarFastPath.hitRate;
  lines.push(
    `Scalar fast-path: ${formatProfileInteger(summary.scalarFastPath.hits)} hits, ` +
      `${formatProfileInteger(summary.scalarFastPath.misses)} misses` +
      (hitRate === null ? "" : ` (${(hitRate * 100).toFixed(1)}% hit rate)`),
  );
  lines.push("");

  const addCountTable = (title: string, rows: CountEntry[]) => {
    lines.push(`## ${title}`, "", "| Rank | Name | Count | Share |", "|---:|---|---:|---:|");
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.name}\` | ${formatProfileInteger(row.count)} | ` +
          `${row.percentage === undefined ? "" : row.percentage.toFixed(1) + "%"} |`,
      );
    });
    lines.push("");
  };

  const addFunctionTable = (title: string, rows: FunctionEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Function | Calls | Self time | Total time | Allocations |",
      "|---:|---|---:|---:|---:|---:|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      const location = row.sourceFile ? ` @ ${row.sourceFile}:${row.line}` : "";
      lines.push(
        `| ${index + 1} | \`${row.name}${location}\` | ${formatProfileInteger(row.calls)} | ` +
          `${formatNanoseconds(row.selfTimeNs)} | ${formatNanoseconds(row.totalTimeNs)} | ` +
          `${formatProfileInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  const addTestTable = (title: string, rows: TestProfileEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Test | Status | Opcodes | Allocations | Duration | Profile |",
      "|---:|---|---|---:|---:|---:|---|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.id}\` | ${row.status} | ${formatProfileInteger(row.totalOpcodes)} | ` +
          `${formatProfileInteger(row.allocations)} | ${row.durationMs}ms | \`${row.profilePath}\` |`,
      );
    });
    lines.push("");
  };

  const addGroupTable = (title: string, rows: ProfileGroupEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | Path | Profiles | Opcodes | Self time | Allocations |",
      "|---:|---|---:|---:|---:|---:|",
    );
    rows.slice(0, 20).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.key}\` | ${formatProfileInteger(row.profiles)} | ` +
          `${formatProfileInteger(row.totalOpcodes)} | ${formatNanoseconds(row.functionSelfTimeNs)} | ` +
          `${formatProfileInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  addCountTable("Top Opcodes", summary.topOpcodes);
  addCountTable("Top Opcode Pairs", summary.topOpcodePairs);
  addFunctionTable("Top Functions By Self Time", summary.topFunctionsBySelfTime);
  addFunctionTable("Top Functions By Calls", summary.topFunctionsByCalls);
  addFunctionTable("Top Functions By Allocations", summary.topFunctionsByAllocations);
  addGroupTable("Category Hot Spots", summary.categoryBreakdown);
  addGroupTable("Path Hot Spots", summary.pathBreakdown);
  addTestTable("Top Tests By Opcodes", summary.topTestsByOpcodes);
  addTestTable("Top Tests By Allocations", summary.topTestsByAllocations);
  addTestTable("Top Tests By Duration", summary.topTestsByDuration);

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, `${lines.join("\n").trimEnd()}\n`);
}

// ---------------------------------------------------------------------------
// Console summary
// ---------------------------------------------------------------------------

function printConsoleSummary(s: SuiteSummary, results: PerTestRecord[]): void {
  console.log();
  console.log("=".repeat(60));
  console.log("test262 Conformance Summary");
  console.log("=".repeat(60));
  console.log(`  Discovered:           ${pad(s.totalDiscovered, 6)}`);
  console.log(`  Run:                  ${pad(s.totalRun, 6)}`);
  console.log(
    `  Passed:               ${pad(s.passed, 6)}  ` +
      `(${pct(s.passed, s.totalRun)})`,
  );
  console.log(`  Failed:               ${pad(s.failed, 6)}`);
  if (s.wrapperInfraFailures > 0) {
    console.log(`  Wrapper infra:        ${pad(s.wrapperInfraFailures, 6)}  *** non-zero blocks merge ***`);
  } else {
    console.log(`  Wrapper infra:        ${pad(s.wrapperInfraFailures, 6)}`);
  }
  if (s.timeouts > 0) console.log(`  Timeouts:             ${pad(s.timeouts, 6)}`);
  console.log(`  Duration:             ${s.durationSeconds.toFixed(1)}s`);
  console.log();

  console.log("Per-category breakdown:");
  console.log(
    "  Category   |    Run | Passed | Failed | Wrap-Infra | Pass-rate",
  );
  console.log(
    "  -----------+--------+--------+--------+------------+----------",
  );
  for (const c of s.byCategory) {
    console.log(
      `  ${padRight(c.category, 10)} | ${pad(c.run, 6)} | ${pad(c.passed, 6)} | ${pad(c.failed, 6)} | ${pad(c.wrapperInfra, 10)} | ${pct(c.passed, c.run)}`,
    );
  }
  console.log();

  const failing = results
    .filter((r) => r.status === "FAIL" || r.status === "TIMEOUT")
    .slice(0, 30);
  if (failing.length > 0) {
    console.log(`Failing tests (showing first 30 of ${results.filter((r) => r.status === "FAIL" || r.status === "TIMEOUT").length}):`);
    for (const r of failing) {
      console.log(`  [${r.status}] ${r.id}`);
      if (r.message) console.log(`         ${r.message.slice(0, 200)}`);
    }
    console.log();
  }
}

function pad(n: number, width: number): string {
  return String(n).padStart(width);
}

function padRight(s: string, width: number): string {
  return s.padEnd(width);
}

function pct(n: number, d: number): string {
  if (d === 0) return "0.0%";
  return ((n / d) * 100).toFixed(1) + "%";
}

// ---------------------------------------------------------------------------
// Step summary (GitHub Actions $GITHUB_STEP_SUMMARY)
// ---------------------------------------------------------------------------

async function emitStepSummary(s: SuiteSummary): Promise<void> {
  const path = process.env.GITHUB_STEP_SUMMARY;
  if (!path) return;
  let md = "## test262 Conformance\n\n";
  if (s.wrapperInfraFailures > 0) {
    md += `**${s.wrapperInfraFailures} wrapper infrastructure failure(s)** — conformance numbers untrustworthy\n\n`;
  } else {
    md += `Wrapper infra failures: 0\n\n`;
  }
  md += "| Category   |    Run | Passed | Failed | Pass-rate |\n";
  md += "|------------|-------:|-------:|-------:|----------:|\n";
  for (const c of s.byCategory) {
    md += `| ${c.category} | ${c.run} | ${c.passed} | ${c.failed} | ${pct(c.passed, c.run)} |\n`;
  }
  md += `| **total** | ${s.totalRun} | ${s.passed} | ${s.failed} | ${pct(s.passed, s.totalRun)} |\n`;
  md += `\nDuration: ${s.durationSeconds.toFixed(1)}s\n`;
  await Bun.write(path, md);
}

// ---------------------------------------------------------------------------
// Shard merge mode
// ---------------------------------------------------------------------------

interface MergeArgs {
  output: string | null;
  profileDir: string | null;
  profileSummaryOutput: string | null;
  profileMarkdownOutput: string | null;
  reports: string[];
}

function parseMergeArgs(argv: string[]): MergeArgs {
  const out: MergeArgs = {
    output: null,
    profileDir: null,
    profileSummaryOutput: null,
    profileMarkdownOutput: null,
    reports: [],
  };
  for (let index = 0; index < argv.length; index++) {
    const arg = argv[index];
    if (arg === "--output") out.output = argv[++index];
    else if (arg.startsWith("--output=")) out.output = arg.slice("--output=".length);
    else if (arg === "--profile-dir") out.profileDir = argv[++index];
    else if (arg.startsWith("--profile-dir=")) out.profileDir = arg.slice("--profile-dir=".length);
    else if (arg === "--profile-summary-output" || arg === "--profile-report-json") out.profileSummaryOutput = argv[++index];
    else if (arg.startsWith("--profile-summary-output=")) out.profileSummaryOutput = arg.slice("--profile-summary-output=".length);
    else if (arg.startsWith("--profile-report-json=")) out.profileSummaryOutput = arg.slice("--profile-report-json=".length);
    else if (arg === "--profile-markdown-output" || arg === "--profile-report-md" || arg === "--profile-report-markdown") out.profileMarkdownOutput = argv[++index];
    else if (arg.startsWith("--profile-markdown-output=")) out.profileMarkdownOutput = arg.slice("--profile-markdown-output=".length);
    else if (arg.startsWith("--profile-report-md=")) out.profileMarkdownOutput = arg.slice("--profile-report-md=".length);
    else if (arg.startsWith("--profile-report-markdown=")) out.profileMarkdownOutput = arg.slice("--profile-report-markdown=".length);
    else if (arg.startsWith("-")) throw new Error(`Unknown merge argument: ${arg}`);
    else out.reports.push(arg);
  }
  if (!out.output) throw new Error("--merge-shards requires --output");
  if (out.reports.length === 0) {
    throw new Error("--merge-shards requires at least one shard report");
  }
  if ((out.profileSummaryOutput || out.profileMarkdownOutput) && !out.profileDir) {
    throw new Error("profile report output options require --profile-dir");
  }
  if (out.profileDir) {
    out.profileSummaryOutput ??= join(out.profileDir, "aggregate.json");
    out.profileMarkdownOutput ??= join(out.profileDir, "aggregate.md");
  }
  return out;
}

function readShardReport(path: string): SuiteReport {
  let report: SuiteReport;
  try {
    report = JSON.parse(readFileSync(path, "utf8")) as SuiteReport;
  } catch (err) {
    throw new Error(`Cannot read shard report ${path}: ${err}`);
  }
  if (!report.summary || !Array.isArray(report.results) ||
      !report.run || !report.shard) {
    throw new Error(`Invalid shard report ${path}: missing summary, results, run, or shard metadata`);
  }
  if (report.summary.totalRun !== report.results.length) {
    throw new Error(
      `Invalid shard report ${path}: summary totalRun ${report.summary.totalRun} ` +
        `does not match ${report.results.length} results`,
    );
  }
  return report;
}

function mergeShardReports(paths: string[]): SuiteReport {
  const reports = paths.map(readShardReport);
  const first = reports[0];
  const shardCount = first.shard!.count;
  if (paths.length !== shardCount) {
    throw new Error(`Expected ${shardCount} shard reports, got ${paths.length}`);
  }

  const runSignature = JSON.stringify({
    test262Sha: first.run!.test262Sha,
    mode: first.run!.mode,
    categories: first.run!.categories,
    jobs: first.run!.jobs,
    timeoutMs: first.run!.timeoutMs,
    maxMemoryBytes: first.run!.maxMemoryBytes,
  });
  const seenShards = new Set<number>();
  const seenTests = new Set<string>();
  const results: PerTestRecord[] = [];
  let durationSeconds = 0;

  for (const [pathIndex, report] of reports.entries()) {
    const path = paths[pathIndex];
    if (report.shard!.count !== shardCount) {
      throw new Error(`Shard count mismatch in ${path}`);
    }
    if (report.shard!.index < 0 || report.shard!.index >= shardCount ||
        seenShards.has(report.shard!.index)) {
      throw new Error(`Duplicate or invalid shard index ${report.shard!.index} in ${path}`);
    }
    seenShards.add(report.shard!.index);
    if (report.summary.totalDiscovered !== first.summary.totalDiscovered) {
      throw new Error(`Discovered-test count mismatch in ${path}`);
    }
    const signature = JSON.stringify({
      test262Sha: report.run!.test262Sha,
      mode: report.run!.mode,
      categories: report.run!.categories,
      jobs: report.run!.jobs,
      timeoutMs: report.run!.timeoutMs,
      maxMemoryBytes: report.run!.maxMemoryBytes,
    });
    if (signature !== runSignature) {
      throw new Error(`Run metadata mismatch in ${path}`);
    }
    durationSeconds = Math.max(durationSeconds, report.summary.durationSeconds);
    for (const result of report.results) {
      if (shardForTest(result.id, shardCount) !== report.shard!.index) {
        throw new Error(`Test ${result.id} is in the wrong shard report: ${path}`);
      }
      if (seenTests.has(result.id)) {
        throw new Error(`Duplicate test result ${result.id}`);
      }
      seenTests.add(result.id);
      results.push(result);
    }
  }

  if (seenShards.size !== shardCount) {
    throw new Error(`Incomplete shard set: expected ${shardCount}, got ${seenShards.size}`);
  }
  if (results.length !== first.summary.totalDiscovered) {
    throw new Error(
      `Incomplete result set: discovered ${first.summary.totalDiscovered}, merged ${results.length}`,
    );
  }

  results.sort((a, b) => a.id.localeCompare(b.id));
  return {
    summary: aggregate(results, durationSeconds, first.summary.totalDiscovered),
    results,
    run: {
      ...first.run!,
      jobs: first.run!.jobs * shardCount,
    },
  };
}

async function runMerge(argv: string[]): Promise<number> {
  const args = parseMergeArgs(argv);
  const report = mergeShardReports(args.reports);
  if (args.profileDir && report.run!.mode !== "bytecode") {
    throw new Error("--profile-dir requires bytecode shard reports");
  }
  mkdirSync(dirname(args.output!), { recursive: true });
  writeFileSync(args.output!, JSON.stringify(report, null, 2) + "\n");
  console.log(`Merged ${args.reports.length} shards into: ${resolve(args.output!)}`);

  if (args.profileDir && (args.profileSummaryOutput || args.profileMarkdownOutput)) {
    const run = report.run!;
    const profileSummary = buildProfileSummary(report.results, args.profileDir, {
      suiteDir: run.suiteDir,
      test262Sha: run.test262Sha,
      mode: "bytecode",
      categories: run.categories,
      jobs: run.jobs,
      timeoutMs: run.timeoutMs,
      maxMemoryBytes: run.maxMemoryBytes,
      totalDiscovered: report.summary.totalDiscovered,
      totalRun: report.summary.totalRun,
      passed: report.summary.passed,
      failed: report.summary.failed,
      wrapperInfraFailures: report.summary.wrapperInfraFailures,
      timeouts: report.summary.timeouts,
      durationSeconds: report.summary.durationSeconds,
    });
    if (args.profileSummaryOutput) {
      mkdirSync(dirname(args.profileSummaryOutput), { recursive: true });
      writeFileSync(
        args.profileSummaryOutput,
        JSON.stringify(profileSummary, null, 2) + "\n",
      );
      console.log(`Profile summary written to: ${resolve(args.profileSummaryOutput)}`);
    }
    if (args.profileMarkdownOutput) {
      writeProfileMarkdown(profileSummary, args.profileMarkdownOutput);
      console.log(`Profile markdown written to: ${resolve(args.profileMarkdownOutput)}`);
    }
  }

  printConsoleSummary(report.summary, report.results);
  await emitStepSummary(report.summary);
  return report.summary.wrapperInfraFailures > 0 ? 1 : 0;
}

// ---------------------------------------------------------------------------
// Comment mode (PR-comment markdown builder)
// ---------------------------------------------------------------------------

function fmt(n: number): string {
  return n.toLocaleString("en-US");
}

function signed(n: number): string {
  if (n > 0) return "+" + fmt(n);
  if (n < 0) return fmt(n);
  return "±0";
}

function signedPp(rateNew: number, rateOld: number | null): string {
  if (rateOld == null) return "🆕";
  const dp = (rateNew - rateOld) * 100;
  if (Math.abs(dp) <= 0.05) return "±0pp";
  return (dp > 0 ? "+" : "") + dp.toFixed(1) + "pp";
}

interface BaselineSummary {
  byCategory?: CategorySummary[];
  totalRun?: number;
  passed?: number;
  failed?: number;
  wrapperInfraFailures?: number;
  timeouts?: number;
}

interface RegressionDelta {
  hasBaseline: boolean;
  /** current.passed - baseline.passed; equals current.passed when hasBaseline is false. */
  totalPassedDelta: number;
  /** Tests that were not PASS in baseline but are PASS in current. */
  newPasses: string[];
  /** Tests that were PASS in baseline but are now non-timeout failures. */
  newFails: string[];
  /** Tests that newly report TIMEOUT compared with baseline. */
  newTimeouts: string[];
  /** Tests that no longer report TIMEOUT compared with baseline. */
  resolvedTimeouts: string[];
}

function computeRegression(
  data: { summary: SuiteSummary; results: PerTestRecord[] } | null,
  baseline: { summary: BaselineSummary; results?: PerTestRecord[] } | null,
): RegressionDelta {
  const hasBaseline = !!(baseline && baseline.summary);
  if (!data) {
    return {
      hasBaseline,
      totalPassedDelta: 0,
      newPasses: [],
      newFails: [],
      newTimeouts: [],
      resolvedTimeouts: [],
    };
  }
  const baseTotalPassed = hasBaseline ? (baseline!.summary.passed ?? 0) : 0;
  const totalPassedDelta = hasBaseline
    ? data.summary.passed - baseTotalPassed
    : data.summary.passed;
  const newPasses: string[] = [];
  const newFails: string[] = [];
  const newTimeouts: string[] = [];
  const resolvedTimeouts: string[] = [];
  if (hasBaseline) {
    const prevById = new Map<string, Outcome>();
    for (const r of baseline!.results || []) prevById.set(r.id, r.status);
    for (const r of data.results) {
      const prev = prevById.get(r.id);
      if (!prev) continue;
      if (prev !== "TIMEOUT" && r.status === "TIMEOUT") newTimeouts.push(r.id);
      else if (prev === "TIMEOUT" && r.status !== "TIMEOUT") {
        resolvedTimeouts.push(r.id);
      } else if (prev !== "PASS" && r.status === "PASS") {
        newPasses.push(r.id);
      } else if (prev === "PASS" && r.status !== "PASS") {
        newFails.push(r.id);
      }
    }
  }
  return { hasBaseline, totalPassedDelta, newPasses, newFails, newTimeouts, resolvedTimeouts };
}

interface AreaBucket {
  key: string;
  attempted: number;
  passed: number;
}

function bucketAreas(results: PerTestRecord[]): Map<string, AreaBucket> {
  const out = new Map<string, AreaBucket>();
  for (const r of results) {
    const parts = r.id.split("/");
    if (parts.length < 2) continue;
    const key = parts.slice(0, 2).join("/");
    let b = out.get(key);
    if (!b) {
      b = { key, attempted: 0, passed: 0 };
      out.set(key, b);
    }
    b.attempted++;
    if (r.status === "PASS") b.passed++;
  }
  return out;
}

function buildCommentMarkdown(
  data: { summary: SuiteSummary; results: PerTestRecord[] } | null,
  baseline: { summary: BaselineSummary; results?: PerTestRecord[] } | null,
): { markdown: string; regressed: boolean } {
  const marker = "<!-- test262-results -->";
  let body = "## test262 Conformance\n\n";

  if (!data || !data.summary) {
    body +=
      "_test262 results were not produced for this run._\n\n" +
      "<sub>Non-blocking. The conformance job either timed out, crashed, or did not upload an artifact. See the workflow run for details.</sub>\n";
    return { markdown: marker + "\n" + body, regressed: false };
  }

  const s = data.summary;
  const delta = computeRegression(data, baseline);
  const hasBaseline = delta.hasBaseline;
  const regressed = hasBaseline && delta.newFails.length > 0;

  if (regressed) {
    body += `> 🚫 **Regression vs cached \`main\` baseline.** ${delta.newFails.length} previously-passing test(s) now fail; pass count Δ ${delta.totalPassedDelta >= 0 ? "+" : ""}${delta.totalPassedDelta}. This run blocks merge — see "Newly failing" below.\n\n`;
  }
  const baselineByCat = new Map<string, CategorySummary>();
  if (hasBaseline) {
    for (const c of baseline!.summary.byCategory || []) {
      baselineByCat.set(c.category, c);
    }
  }

  if (s.wrapperInfraFailures > 0) {
    body += `> ⚠️ **${s.wrapperInfraFailures} wrapper infrastructure failure(s)** — conformance numbers untrustworthy until fixed.\n\n`;
  }
  if (s.timeouts > 0) {
    body += `> **${s.timeouts} timeout(s)** — counted separately from ordinary conformance failures; inspect the full artifact for affected tests.\n\n`;
  }

  body += "| Category | Run | Passed | Δ Pass | Failed | Timeouts | Δ Timeout | Wrap-Infra | Pass-rate | Δ Rate |\n";
  body += "|----------|----:|-------:|-------:|-------:|---------:|----------:|-----------:|----------:|-------:|\n";
  for (const c of s.byCategory) {
    const rate = c.run > 0 ? c.passed / c.run : 0;
    const ba = baselineByCat.get(c.category);
    const dPass = ba ? c.passed - ba.passed : c.passed;
    const dTimeout = ba ? c.timeouts - (ba.timeouts ?? 0) : c.timeouts;
    const baseRate = ba && ba.run > 0 ? ba.passed / ba.run : null;
    body += `| \`${c.category}\` | ${fmt(c.run)} | ${fmt(c.passed)} | ${ba ? signed(dPass) : "🆕"} | ${fmt(c.failed)} | ${fmt(c.timeouts)} | ${ba ? signed(dTimeout) : "🆕"} | ${fmt(c.wrapperInfra)} | ${pct(c.passed, c.run)} | ${signedPp(rate, baseRate)} |\n`;
  }
  // Totals row
  const totalRate = s.totalRun > 0 ? s.passed / s.totalRun : 0;
  const baseTotalRun = (baseline?.summary.totalRun) ?? 0;
  const baseTotalPassed = (baseline?.summary.passed) ?? 0;
  const baseTotalTimeouts = (baseline?.summary.timeouts) ?? 0;
  const baseTotalRate = baseTotalRun > 0 ? baseTotalPassed / baseTotalRun : null;
  body += `| **total** | ${fmt(s.totalRun)} | ${fmt(s.passed)} | ${hasBaseline ? signed(delta.totalPassedDelta) : "🆕"} | ${fmt(s.failed)} | ${fmt(s.timeouts)} | ${hasBaseline ? signed(s.timeouts - baseTotalTimeouts) : "🆕"} | ${fmt(s.wrapperInfraFailures)} | ${pct(s.passed, s.totalRun)} | ${signedPp(totalRate, baseTotalRate)} |\n\n`;

  // Areas closest to 100%
  const MIN_SAMPLE = 25;
  const areas = bucketAreas(data.results);
  const baselineAreas = hasBaseline ? bucketAreas(baseline!.results || []) : new Map();
  const ranked = Array.from(areas.values())
    .filter((a) => a.attempted >= MIN_SAMPLE && a.passed < a.attempted)
    .map((a) => ({ ...a, rate: a.passed / a.attempted }))
    .sort(
      (x, y) =>
        y.rate - x.rate ||
        y.attempted - x.attempted ||
        x.key.localeCompare(y.key),
    )
    .slice(0, 3);

  if (ranked.length > 0) {
    body += "### Areas closest to 100%\n\n";
    if (hasBaseline) {
      body += "| Area | Pass rate | Δ vs main | Passing |\n";
      body += "|------|-----------|-----------|---------|\n";
      for (const a of ranked) {
        const ba = baselineAreas.get(a.key);
        const baseRate =
          ba && ba.attempted > 0 ? ba.passed / ba.attempted : null;
        body += `| \`${a.key}\` | ${pct(a.passed, a.attempted)} | ${signedPp(a.rate, baseRate)} | ${fmt(a.passed)} / ${fmt(a.attempted)} |\n`;
      }
    } else {
      body += "| Area | Pass rate | Passing |\n";
      body += "|------|-----------|---------|\n";
      for (const a of ranked) {
        body += `| \`${a.key}\` | ${pct(a.passed, a.attempted)} | ${fmt(a.passed)} / ${fmt(a.attempted)} |\n`;
      }
    }
    body += "\n";
  } else {
    body +=
      "_No areas yet meet the highlight criteria (≥ 25 attempted tests, below 100%)._\n\n";
  }

  // Per-test deltas (collapsible)
  if (
    hasBaseline &&
    (delta.newPasses.length > 0 ||
      delta.newFails.length > 0 ||
      delta.newTimeouts.length > 0 ||
      delta.resolvedTimeouts.length > 0)
  ) {
    body += `<details${regressed ? " open" : ""}>\n<summary>Per-test deltas (+${delta.newPasses.length} / -${delta.newFails.length} / timeout +${delta.newTimeouts.length} / -${delta.resolvedTimeouts.length})</summary>\n\n`;
    if (delta.newFails.length > 0) {
      body += `**Newly failing (${delta.newFails.length}):**\n\n`;
      for (const id of delta.newFails.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newFails.length > 100) body += `- _… ${delta.newFails.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.newPasses.length > 0) {
      body += `**Newly passing (${delta.newPasses.length}):**\n\n`;
      for (const id of delta.newPasses.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newPasses.length > 100) body += `- _… ${delta.newPasses.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.newTimeouts.length > 0) {
      body += `**New timeouts (${delta.newTimeouts.length}):**\n\n`;
      for (const id of delta.newTimeouts.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.newTimeouts.length > 100) body += `- _… ${delta.newTimeouts.length - 100} more_\n`;
      body += "\n";
    }
    if (delta.resolvedTimeouts.length > 0) {
      body += `**Resolved timeouts (${delta.resolvedTimeouts.length}):**\n\n`;
      for (const id of delta.resolvedTimeouts.slice(0, 100)) body += `- \`${id}\`\n`;
      if (delta.resolvedTimeouts.length > 100) body += `- _… ${delta.resolvedTimeouts.length - 100} more_\n`;
      body += "\n";
    }
    body += "</details>\n\n";
  }

  const baselineNote = hasBaseline
    ? " Δ vs main compares against the most recent cached `main` baseline."
    : " No `main` baseline cached yet — Δ columns will appear once a `main` run completes.";
  body += `<sub>Steady-state failures and timeouts are non-blocking; PASS → non-timeout failure transitions fail the conformance gate. Measured on ubuntu-latest x64, bytecode mode. Areas grouped by the first two test262 path components; minimum 25 attempted tests, areas already at 100% excluded.${baselineNote}</sub>\n`;

  return { markdown: marker + "\n" + body, regressed };
}

function runComment(argv: string[]): number {
  if (argv.length < 2) {
    console.error("Usage: --comment <results.json> <baseline.json|->");
    return 2;
  }
  const [resultsPath, baselinePath] = argv;
  let data: any = null;
  try {
    data = JSON.parse(readFileSync(resultsPath, "utf8"));
  } catch (e) {
    console.error(`Failed to read ${resultsPath}: ${(e as Error).message}`);
    // Still emit a markdown stub so the PR comment shows the skip.
  }
  let baseline: any = null;
  if (baselinePath !== "-") {
    try {
      baseline = JSON.parse(readFileSync(baselinePath, "utf8"));
    } catch {
      // Missing baseline is fine — surfaces as 🆕 in the table.
    }
  }
  const { markdown, regressed } = buildCommentMarkdown(data, baseline);
  process.stdout.write(markdown);
  if (regressed) {
    // Single-line CI annotation; the per-test list is already in the
    // markdown comment posted to the PR, so don't repeat it here.
    console.error(
      "::error title=test262 regression::A previously-passing test now fails. See the PR comment for per-test deltas.",
    );
    return 1;
  }
  return 0;
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

const argv = process.argv.slice(2);
if (argv[0] === "--comment") {
  process.exit(runComment(argv.slice(1)));
} else if (argv[0] === "--merge-shards") {
  runMerge(argv.slice(1)).then(
    (code) => process.exit(code),
    (err) => {
      console.error(err);
      process.exit(1);
    },
  );
} else {
  console.error(
    "Test execution moved to ./build/GocciaTest262Runner. " +
      "This script now owns only shard merging and PR comment rendering.",
  );
  process.exit(2);
}
