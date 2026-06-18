#!/usr/bin/env bun
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "fs";
import { dirname, join, relative, resolve } from "path";

const REPORT_LIMIT = 50;
const MARKDOWN_REPORT_LIMIT = 20;

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
type FileEntry = {
  file: string;
  totalOpcodes: number;
  allocations: number;
  functionSelfTimeNs: number;
};
type GroupEntry = FileEntry & { profiles: number };

type BenchmarkProfileSummary = {
  generatedAt: string;
  run: {
    commit: string | null;
    mode: "bytecode";
    deterministic: true;
    profileDir: string;
    profileCount: number;
  };
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
  topFilesByOpcodes: FileEntry[];
  topFilesByAllocations: FileEntry[];
  topFilesBySelfTime: FileEntry[];
  pathBreakdown: GroupEntry[];
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
    .slice(0, REPORT_LIMIT);
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
    .slice(0, REPORT_LIMIT);
}

function topFiles(
  files: FileEntry[],
  metric: keyof Pick<FileEntry, "totalOpcodes" | "allocations" | "functionSelfTimeNs">,
): FileEntry[] {
  return [...files]
    .sort((a, b) => b[metric] - a[metric] || a.file.localeCompare(b.file))
    .slice(0, REPORT_LIMIT);
}

function addGroup(
  groups: Map<string, GroupEntry>,
  key: string,
  profile: ProfileJson,
): void {
  const entry =
    groups.get(key) ??
    ({
      file: key,
      profiles: 0,
      totalOpcodes: 0,
      allocations: 0,
      functionSelfTimeNs: 0,
    } satisfies GroupEntry);
  entry.profiles++;
  entry.totalOpcodes += totalOpcodeCount(profile);
  entry.allocations += totalAllocations(profile);
  entry.functionSelfTimeNs += totalFunctionSelfTime(profile);
  groups.set(key, entry);
}

function topGroups(groups: Map<string, GroupEntry>): GroupEntry[] {
  return [...groups.values()]
    .sort(
      (a, b) =>
        b.totalOpcodes - a.totalOpcodes ||
        b.allocations - a.allocations ||
        a.file.localeCompare(b.file),
    )
    .slice(0, REPORT_LIMIT);
}

function walkJsonFiles(dir: string, out: string[] = []): string[] {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) walkJsonFiles(path, out);
    else if (entry.isFile() && entry.name.endsWith(".json")) out.push(path);
  }
  return out;
}

function readProfile(path: string): ProfileJson {
  let parsed: unknown;
  try {
    parsed = JSON.parse(readFileSync(path, "utf8")) as unknown;
  } catch (err) {
    throw new Error(`Failed to parse benchmark profile JSON ${path}: ${(err as Error).message}`);
  }
  if (!isProfileJson(parsed)) {
    throw new Error(`Invalid benchmark profile JSON ${path}: expected Goccia profiler report object`);
  }
  return parsed;
}

function isProfileJson(value: unknown): value is ProfileJson {
  if (!value || typeof value !== "object" || Array.isArray(value)) return false;
  const profile = value as Record<string, unknown>;
  const hasRecognizedProfileField =
    "opcodes" in profile ||
    "opcodePairs" in profile ||
    "scalarFastPath" in profile ||
    "shapeSaturation" in profile ||
    "functions" in profile;
  if (!hasRecognizedProfileField) return false;
  return (
    (profile.opcodes === undefined || Array.isArray(profile.opcodes)) &&
    (profile.opcodePairs === undefined || Array.isArray(profile.opcodePairs)) &&
    (profile.functions === undefined || Array.isArray(profile.functions)) &&
    (profile.scalarFastPath === undefined ||
      (typeof profile.scalarFastPath === "object" &&
        profile.scalarFastPath !== null &&
        !Array.isArray(profile.scalarFastPath))) &&
    (profile.shapeSaturation === undefined ||
      (typeof profile.shapeSaturation === "object" &&
        profile.shapeSaturation !== null &&
        !Array.isArray(profile.shapeSaturation)))
  );
}

function buildSummary(profileDir: string): BenchmarkProfileSummary {
  const resolvedProfileDir = resolve(profileDir);
  const paths = walkJsonFiles(resolvedProfileDir).sort();
  const opcodeCounts = new Map<string, number>();
  const opcodePairCounts = new Map<string, number>();
  const functionTotals = new Map<string, FunctionEntry>();
  const groups = new Map<string, GroupEntry>();
  const files: FileEntry[] = [];
  let totalOpcodes = 0;
  let totalOpcodePairs = 0;
  let scalarHits = 0;
  let scalarMisses = 0;
  let depthLimitPrefixes = 0;
  let tableCapacityEvents = 0;

  for (const path of paths) {
    const profile = readProfile(path);
    const file = relative(resolvedProfileDir, path).split("\\").join("/");
    const fileOpcodes = totalOpcodeCount(profile);
    const allocations = totalAllocations(profile);
    const functionSelfTimeNs = totalFunctionSelfTime(profile);
    totalOpcodes += fileOpcodes;
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

    files.push({ file, totalOpcodes: fileOpcodes, allocations, functionSelfTimeNs });
    addGroup(groups, file.split("/")[0] || "root", profile);
  }

  const scalarTotal = scalarHits + scalarMisses;
  return {
    generatedAt: new Date().toISOString(),
    run: {
      commit: process.env.GITHUB_SHA ?? process.env.BENCHMARK_PROFILE_COMMIT ?? null,
      mode: "bytecode",
      deterministic: true,
      profileDir: resolvedProfileDir,
      profileCount: paths.length,
    },
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
    topFilesByOpcodes: topFiles(files, "totalOpcodes"),
    topFilesByAllocations: topFiles(files, "allocations"),
    topFilesBySelfTime: topFiles(files, "functionSelfTimeNs"),
    pathBreakdown: topGroups(groups),
  };
}

function formatInteger(value: number): string {
  return Math.round(value).toLocaleString("en-US");
}

function formatMs(ns: number): string {
  const ms = ns / 1e6;
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`;
  return `${ms.toFixed(1)}ms`;
}

function writeMarkdown(summary: BenchmarkProfileSummary, outputPath: string): void {
  const lines: string[] = [];
  lines.push("# Benchmark Profile Report", "");
  lines.push(`Generated: ${summary.generatedAt}`);
  lines.push(`Commit: ${summary.run.commit ?? "unknown"}`);
  lines.push("Mode: bytecode");
  lines.push("Deterministic: true");
  lines.push(`Profiles: ${formatInteger(summary.run.profileCount)}`);
  lines.push(`Total opcodes: ${formatInteger(summary.totalOpcodes)}`);
  lines.push(`Total opcode pairs: ${formatInteger(summary.totalOpcodePairs)}`);
  const hitRate = summary.scalarFastPath.hitRate;
  lines.push(
    `Scalar fast-path: ${formatInteger(summary.scalarFastPath.hits)} hits, ` +
      `${formatInteger(summary.scalarFastPath.misses)} misses` +
      (hitRate === null ? "" : ` (${(hitRate * 100).toFixed(1)}% hit rate)`),
  );
  lines.push("");

  const addCountTable = (title: string, rows: CountEntry[]) => {
    lines.push(`## ${title}`, "", "| Rank | Name | Count | Share |", "|---:|---|---:|---:|");
    rows.slice(0, MARKDOWN_REPORT_LIMIT).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.name}\` | ${formatInteger(row.count)} | ` +
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
    rows.slice(0, MARKDOWN_REPORT_LIMIT).forEach((row, index) => {
      const location = row.sourceFile ? ` @ ${row.sourceFile}:${row.line}` : "";
      lines.push(
        `| ${index + 1} | \`${row.name}${location}\` | ${formatInteger(row.calls)} | ` +
          `${formatMs(row.selfTimeNs)} | ${formatMs(row.totalTimeNs)} | ` +
          `${formatInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  const addFileTable = (title: string, rows: FileEntry[]) => {
    lines.push(
      `## ${title}`,
      "",
      "| Rank | File | Opcodes | Self time | Allocations |",
      "|---:|---|---:|---:|---:|",
    );
    rows.slice(0, MARKDOWN_REPORT_LIMIT).forEach((row, index) => {
      lines.push(
        `| ${index + 1} | \`${row.file}\` | ${formatInteger(row.totalOpcodes)} | ` +
          `${formatMs(row.functionSelfTimeNs)} | ${formatInteger(row.allocations)} |`,
      );
    });
    lines.push("");
  };

  addCountTable("Top Opcodes", summary.topOpcodes);
  addCountTable("Top Opcode Pairs", summary.topOpcodePairs);
  addFunctionTable("Top Functions By Self Time", summary.topFunctionsBySelfTime);
  addFunctionTable("Top Functions By Calls", summary.topFunctionsByCalls);
  addFunctionTable("Top Functions By Allocations", summary.topFunctionsByAllocations);
  addFileTable("Top Files By Opcodes", summary.topFilesByOpcodes);
  addFileTable("Top Files By Allocations", summary.topFilesByAllocations);
  addFileTable("Top Files By Self Time", summary.topFilesBySelfTime);

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, `${lines.join("\n").trimEnd()}\n`);
}

function parseArgs(argv: string[]): {
  profileDir: string;
  summaryOutput: string;
  markdownOutput: string | null;
} {
  let profileDir: string | null = null;
  let summaryOutput: string | null = null;
  let markdownOutput: string | null = null;
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--profile-dir") profileDir = argv[++i] ?? null;
    else if (arg.startsWith("--profile-dir=")) {
      profileDir = arg.slice("--profile-dir=".length);
    } else if (arg === "--summary-output") summaryOutput = argv[++i] ?? null;
    else if (arg.startsWith("--summary-output=")) {
      summaryOutput = arg.slice("--summary-output=".length);
    } else if (arg === "--markdown-output") markdownOutput = argv[++i] ?? null;
    else if (arg.startsWith("--markdown-output=")) {
      markdownOutput = arg.slice("--markdown-output=".length);
    } else if (arg === "--help" || arg === "-h") {
      usage();
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  if (!profileDir || !summaryOutput) usage();
  return { profileDir, summaryOutput, markdownOutput };
}

function usage(): never {
  console.error(`Usage:
  bun scripts/aggregate_benchmark_profiles.ts --profile-dir profile-baseline --summary-output benchmark-profile-aggregate.json [--markdown-output benchmark-profile-aggregate.md]
`);
  process.exit(2);
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  if (!existsSync(args.profileDir)) {
    throw new Error(`Profile directory not found: ${args.profileDir}`);
  }
  const summary = buildSummary(args.profileDir);
  mkdirSync(dirname(args.summaryOutput), { recursive: true });
  writeFileSync(args.summaryOutput, `${JSON.stringify(summary, null, 2)}\n`);
  if (args.markdownOutput) writeMarkdown(summary, args.markdownOutput);
  console.log(`Benchmark profile summary written to: ${resolve(args.summaryOutput)}`);
  if (args.markdownOutput) {
    console.log(`Benchmark profile markdown written to: ${resolve(args.markdownOutput)}`);
  }
}

main();
