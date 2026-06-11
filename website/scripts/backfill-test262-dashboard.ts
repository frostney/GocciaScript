#!/usr/bin/env bun
import { access, mkdir, mkdtemp, readFile, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { GITHUB_REPO_URL } from "../src/lib/github";
import {
  extractJsonFromZip,
  jsonUrlForArtifact,
  normalizeTest262Report,
  type Test262CategorySummary,
  type Test262Outcome,
  type Test262Report,
  type Test262TimelinePoint,
  test262ReportFileName,
} from "../src/lib/test262-dashboard";
import {
  loadTest262BlobManifest,
  publishTest262ReportsToBlob,
  type Test262BlobPublishEntry,
} from "./test262-blob-store";

const GITHUB_API_ROOT = "https://api.github.com/repos/frostney/GocciaScript";
const TEST262_WORKFLOW = "ci.yml";
const TEST262_ARTIFACT_NAME = "test262-results";
const WORKFLOW_RUNS_PER_PAGE = 100;
const DEFAULT_SINCE = "2026-05-01";
const DEFAULT_JOBS = 2;
const REQUEST_TIMEOUT_MS = 30_000;
const INTERNAL_PAGE_LIMIT =
  Number(process.env.TEST262_BACKFILL_PAGE_LIMIT) || 50;

type GitHubWorkflowRun = {
  id?: number;
  run_number?: number;
  display_title?: string;
  head_sha?: string;
  html_url?: string;
  head_branch?: string;
  created_at?: string;
  updated_at?: string;
};

type GitHubWorkflowRunsResponse = {
  workflow_runs?: GitHubWorkflowRun[];
};

type GitHubArtifact = {
  id?: number;
  name?: string;
  expired?: boolean;
  archive_download_url?: string;
  created_at?: string;
  workflow_run?: {
    head_sha?: string;
  };
};

type GitHubArtifactsResponse = {
  artifacts?: GitHubArtifact[];
};

type ArtifactCandidate = {
  day: string;
  run: Required<Pick<GitHubWorkflowRun, "id">> & GitHubWorkflowRun;
  artifact: Required<Pick<GitHubArtifact, "id" | "archive_download_url">> &
    GitHubArtifact;
};

type DailyRunGroup = {
  day: string;
  runs: GitHubWorkflowRun[];
};

type BackfillOptions = {
  since: string;
  until: string;
  jobs: number;
  dryRun: boolean;
  force: boolean;
  keepWorktree: boolean;
  artifactsOnly: boolean;
  historyOnly: boolean;
};

const here = path.dirname(fileURLToPath(import.meta.url));
const websiteRoot = path.resolve(here, "..");
const repoRoot = path.resolve(websiteRoot, "..");

function log(message: string) {
  console.log(`[backfill-test262] ${message}`);
}

function githubToken(): string | undefined {
  return process.env.GITHUB_TOKEN || process.env.GITHUB_ARTIFACT_TOKEN;
}

function githubHeaders(requireToken = false): HeadersInit | null {
  const headers: Record<string, string> = {
    Accept: "application/vnd.github+json",
    "X-GitHub-Api-Version": "2022-11-28",
    "User-Agent": "gocciascript-test262-dashboard-backfill",
  };
  const token = githubToken();
  if (token) headers.Authorization = `Bearer ${token}`;
  if (requireToken && !token) return null;
  return headers;
}

function requestSummary(response: Response, started: number): string {
  const elapsed = Date.now() - started;
  const remaining = response.headers.get("x-ratelimit-remaining");
  const rate = remaining ? `, rate remaining ${remaining}` : "";
  return `HTTP ${response.status} in ${elapsed}ms${rate}`;
}

async function fetchGithubJson<T>(url: string, label: string): Promise<T> {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), REQUEST_TIMEOUT_MS);
  const started = Date.now();
  log(`${label}...`);
  try {
    const response = await fetch(url, {
      headers: githubHeaders() ?? undefined,
      signal: controller.signal,
    });
    const text = await response.text();
    log(`${label}: ${requestSummary(response, started)}`);
    if (!response.ok) {
      throw new Error(`${url} HTTP ${response.status} ${response.statusText}`);
    }
    return JSON.parse(text) as T;
  } catch (err) {
    const elapsed = Date.now() - started;
    if (err instanceof Error && err.name === "AbortError") {
      throw new Error(`${label} timed out after ${elapsed}ms`);
    }
    throw err;
  } finally {
    clearTimeout(timeout);
  }
}

function todayUtcDay(): string {
  return new Date().toISOString().slice(0, 10);
}

function utcDayKey(value: string | undefined): string | null {
  if (!value) return null;
  const time = Date.parse(value);
  if (Number.isNaN(time)) return null;
  return new Date(time).toISOString().slice(0, 10);
}

function inRange(day: string, options: BackfillOptions): boolean {
  return day >= options.since && day <= options.until;
}

function optionValue(args: string[], name: string): string | null {
  const prefix = `--${name}=`;
  return (
    args.find((arg) => arg.startsWith(prefix))?.slice(prefix.length) ?? null
  );
}

function parseDateOption(
  args: string[],
  name: string,
  fallback: string,
): string {
  const value = optionValue(args, name) ?? fallback;
  if (!/^\d{4}-\d{2}-\d{2}$/.test(value)) {
    throw new Error(`--${name} must be YYYY-MM-DD`);
  }
  return value;
}

function parseNumberOption(args: string[], name: string, fallback: number) {
  const value = optionValue(args, name);
  if (!value) return fallback;
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`--${name} must be a positive integer`);
  }
  return parsed;
}

function parseOptions(args: string[]): BackfillOptions {
  if (args.includes("--help")) usage();
  const since = parseDateOption(args, "since", DEFAULT_SINCE);
  const until = parseDateOption(args, "until", todayUtcDay());
  if (until < since) {
    throw new Error(
      `--until (${until}) must be on or after --since (${since})`,
    );
  }
  if (args.includes("--artifacts-only") && args.includes("--history-only")) {
    throw new Error("--artifacts-only and --history-only cannot be combined");
  }
  return {
    since,
    until,
    jobs: parseNumberOption(args, "jobs", DEFAULT_JOBS),
    dryRun: args.includes("--dry-run"),
    force: args.includes("--force"),
    keepWorktree: args.includes("--keep-worktree"),
    artifactsOnly: args.includes("--artifacts-only"),
    historyOnly: args.includes("--history-only"),
  };
}

async function fetchWorkflowRuns(
  options: BackfillOptions,
): Promise<GitHubWorkflowRun[]> {
  const runs: GitHubWorkflowRun[] = [];
  for (let page = 1; page <= INTERNAL_PAGE_LIMIT; page++) {
    const url =
      `${GITHUB_API_ROOT}/actions/workflows/${TEST262_WORKFLOW}/runs` +
      `?branch=main&event=push&status=completed&per_page=${WORKFLOW_RUNS_PER_PAGE}&page=${page}`;
    const data = await fetchGithubJson<GitHubWorkflowRunsResponse>(
      url,
      `fetch workflow runs page ${page}`,
    );
    const pageRuns =
      data.workflow_runs?.filter(
        (run) =>
          typeof run.id === "number" &&
          typeof run.head_sha === "string" &&
          run.head_branch === "main",
      ) ?? [];
    log(`page ${page}: ${pageRuns.length} completed main push runs`);
    if (pageRuns.length === 0) break;

    runs.push(...pageRuns);
    const oldestDay = pageRuns
      .map((run) => utcDayKey(run.created_at))
      .filter((day): day is string => day !== null)
      .sort()[0];
    if (
      pageRuns.length < WORKFLOW_RUNS_PER_PAGE ||
      (oldestDay && oldestDay < options.since)
    ) {
      break;
    }
  }
  return runs.sort(
    (a, b) => Date.parse(b.created_at ?? "") - Date.parse(a.created_at ?? ""),
  );
}

function groupRunsByDay(
  runs: GitHubWorkflowRun[],
  options: BackfillOptions,
): DailyRunGroup[] {
  const groups = new Map<string, GitHubWorkflowRun[]>();
  for (const run of runs) {
    const day = utcDayKey(run.created_at);
    if (!day || !inRange(day, options)) continue;
    const group = groups.get(day) ?? [];
    group.push(run);
    groups.set(day, group);
  }
  return [...groups.entries()]
    .map(([day, dayRuns]) => ({ day, runs: dayRuns }))
    .sort((a, b) => a.day.localeCompare(b.day));
}

async function loadExistingDays(
  options: BackfillOptions,
): Promise<Set<string>> {
  if (options.force || !process.env.BLOB_READ_WRITE_TOKEN) return new Set();
  const manifest = await loadTest262BlobManifest();
  return new Set(
    (manifest?.daily ?? []).map((run) => run.createdAt.slice(0, 10)),
  );
}

async function findTest262Artifact(
  run: GitHubWorkflowRun,
): Promise<ArtifactCandidate | null> {
  if (typeof run.id !== "number") return null;
  const data = await fetchGithubJson<GitHubArtifactsResponse>(
    `${GITHUB_API_ROOT}/actions/runs/${run.id}/artifacts`,
    `fetch artifacts for run #${run.run_number ?? run.id}`,
  );
  const artifact = data.artifacts?.find(
    (entry) =>
      entry.name === TEST262_ARTIFACT_NAME &&
      !entry.expired &&
      typeof entry.id === "number" &&
      typeof entry.archive_download_url === "string",
  );
  if (
    !artifact ||
    typeof artifact.id !== "number" ||
    typeof artifact.archive_download_url !== "string"
  ) {
    return null;
  }
  return {
    day: utcDayKey(run.created_at) ?? "unknown-day",
    run: { ...run, id: run.id },
    artifact: {
      ...artifact,
      id: artifact.id,
      archive_download_url: artifact.archive_download_url,
    },
  };
}

async function findDailyArtifactCandidate(
  group: DailyRunGroup,
): Promise<ArtifactCandidate | null> {
  for (const run of group.runs) {
    const candidate = await findTest262Artifact(run);
    if (candidate) return candidate;
    log(
      `${group.day}: run #${run.run_number ?? run.id} has no unexpired ${TEST262_ARTIFACT_NAME} artifact`,
    );
  }
  return null;
}

async function downloadReportJson(artifactId: number): Promise<string> {
  const headers = githubHeaders(true);
  if (!headers) {
    throw new Error(
      "Set GITHUB_TOKEN or GITHUB_ARTIFACT_TOKEN to read GitHub artifact ZIPs",
    );
  }
  const label = `download artifact ${artifactId} ZIP`;
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), REQUEST_TIMEOUT_MS);
  const started = Date.now();
  log(`${label}...`);
  try {
    const response = await fetch(
      `${GITHUB_API_ROOT}/actions/artifacts/${artifactId}/zip`,
      { headers, signal: controller.signal },
    );
    if (!response.ok) {
      log(`${label}: ${requestSummary(response, started)}`);
      throw new Error(
        `artifact ${artifactId} ZIP HTTP ${response.status} ${response.statusText}`,
      );
    }
    const bytes = new Uint8Array(await response.arrayBuffer());
    log(
      `${label}: ${requestSummary(response, started)}, downloaded ${bytes.byteLength} bytes`,
    );
    return extractJsonFromZip(bytes, test262ReportFileName());
  } catch (err) {
    const elapsed = Date.now() - started;
    if (err instanceof Error && err.name === "AbortError") {
      throw new Error(`${label} timed out after ${elapsed}ms`);
    }
    throw err;
  } finally {
    clearTimeout(timeout);
  }
}

function pointFromCandidate(
  candidate: ArtifactCandidate,
  report: Test262Report,
): Test262TimelinePoint {
  const runNumber =
    typeof candidate.run.run_number === "number" ? candidate.run.run_number : 0;
  const headSha =
    candidate.run.head_sha ??
    candidate.artifact.workflow_run?.head_sha ??
    "unknown";
  return {
    runId: candidate.run.id,
    runNumber,
    title: candidate.run.display_title ?? `CI #${runNumber}`,
    headSha,
    shortSha: headSha.slice(0, 8),
    runUrl:
      candidate.run.html_url ??
      `${GITHUB_REPO_URL}/actions/runs/${candidate.run.id}`,
    createdAt:
      candidate.run.created_at ??
      candidate.artifact.created_at ??
      new Date(0).toISOString(),
    updatedAt:
      candidate.run.updated_at ??
      candidate.artifact.created_at ??
      new Date(0).toISOString(),
    artifactId: candidate.artifact.id,
    artifactCreatedAt:
      candidate.artifact.created_at ??
      candidate.run.updated_at ??
      new Date(0).toISOString(),
    jsonUrl: jsonUrlForArtifact(candidate.artifact.id),
    summary: report.summary,
  };
}

function pointFromRun(
  run: GitHubWorkflowRun,
  report: Test262Report,
): Test262TimelinePoint {
  const runId =
    typeof run.id === "number" && Number.isSafeInteger(run.id)
      ? run.id
      : Date.parse(run.created_at ?? "") || Date.now();
  const runNumber = typeof run.run_number === "number" ? run.run_number : 0;
  const headSha = run.head_sha ?? "unknown";
  return {
    runId,
    runNumber,
    title: run.display_title ?? `CI #${runNumber}`,
    headSha,
    shortSha: headSha.slice(0, 8),
    runUrl: run.html_url ?? `${GITHUB_REPO_URL}/actions/runs/${runId}`,
    createdAt: run.created_at ?? new Date(0).toISOString(),
    updatedAt: run.updated_at ?? run.created_at ?? new Date(0).toISOString(),
    artifactId: runId,
    artifactCreatedAt: new Date().toISOString(),
    jsonUrl: jsonUrlForArtifact(runId),
    summary: report.summary,
  };
}

function isObject(value: unknown): value is Record<string, unknown> {
  return !!value && typeof value === "object";
}

function asNumber(value: unknown): number {
  return typeof value === "number" && Number.isFinite(value) ? value : 0;
}

function legacyStatus(value: unknown): Test262Outcome | null {
  if (value === "PASS" || value === "FAIL" || value === "TIMEOUT") {
    return value;
  }
  if (value === "ERROR" || value === "WRAPPER_INFRA") return "WRAPPER_INFRA";
  return null;
}

function aggregateCategories(results: Test262Report["results"]) {
  const categories = new Map<string, Test262CategorySummary>();
  for (const result of results) {
    const category = result.id.split("/")[0] || "unknown";
    const summary =
      categories.get(category) ??
      ({
        category,
        run: 0,
        passed: 0,
        failed: 0,
        wrapperInfra: 0,
        timeouts: 0,
      } satisfies Test262CategorySummary);
    summary.run++;
    if (result.status === "PASS") summary.passed++;
    else if (result.status === "FAIL") summary.failed++;
    else if (result.status === "WRAPPER_INFRA") summary.wrapperInfra++;
    else if (result.status === "TIMEOUT") summary.timeouts++;
    categories.set(category, summary);
  }
  return [...categories.values()].sort((a, b) =>
    a.category.localeCompare(b.category),
  );
}

function normalizeLegacyTest262Report(value: unknown): Test262Report | null {
  if (
    !isObject(value) ||
    !isObject(value.summary) ||
    !Array.isArray(value.results)
  ) {
    return null;
  }
  const results = value.results
    .map((entry): Test262Report["results"][number] | null => {
      if (!isObject(entry) || typeof entry.id !== "string") return null;
      const status = legacyStatus(entry.status);
      if (!status) return null;
      return {
        id: entry.id,
        status,
        durationMs:
          typeof entry.duration_ms === "number"
            ? entry.duration_ms
            : typeof entry.durationMs === "number"
              ? entry.durationMs
              : undefined,
        message: typeof entry.message === "string" ? entry.message : undefined,
        diagnostic:
          typeof entry.diagnostic === "string" ? entry.diagnostic : undefined,
      };
    })
    .filter(
      (entry): entry is Test262Report["results"][number] => entry !== null,
    );

  const summary = value.summary;
  const passed =
    asNumber(summary.passed) ||
    results.filter((r) => r.status === "PASS").length;
  const failed =
    asNumber(summary.failed) ||
    results.filter((r) => r.status === "FAIL").length;
  const wrapperInfraFailures =
    asNumber(summary.wrapperInfraFailures) ||
    asNumber(summary.errors) ||
    results.filter((r) => r.status === "WRAPPER_INFRA").length;
  const timeouts =
    asNumber(summary.timeouts) ||
    results.filter((r) => r.status === "TIMEOUT").length;

  return {
    summary: {
      totalDiscovered:
        asNumber(summary.totalDiscovered) ||
        asNumber(summary.total_discovered) ||
        results.length,
      totalRun:
        asNumber(summary.totalRun) ||
        asNumber(summary.total_run) ||
        results.length,
      passed,
      failed,
      wrapperInfraFailures,
      timeouts,
      durationSeconds:
        asNumber(summary.durationSeconds) || asNumber(summary.duration_seconds),
      byCategory: aggregateCategories(results),
    },
    results,
  };
}

function normalizeAnyTest262Report(
  value: unknown,
): { report: Test262Report; reportJson: string } | null {
  const legacy =
    isObject(value) &&
    isObject(value.summary) &&
    ("total_discovered" in value.summary ||
      "duration_seconds" in value.summary ||
      (Array.isArray(value.results) &&
        value.results.some(
          (entry) => isObject(entry) && "duration_ms" in entry,
        )));
  const report = legacy
    ? normalizeLegacyTest262Report(value)
    : normalizeTest262Report(value);
  if (!report) return null;
  return {
    report,
    reportJson: `${JSON.stringify(report, null, 2)}\n`,
  };
}

async function publishEntry(entry: Test262BlobPublishEntry, label: string) {
  const manifest = await publishTest262ReportsToBlob([entry]);
  log(
    `${label}: published; manifest now has ${manifest.runs.length} run(s) and ${manifest.daily.length} daily point(s)`,
  );
}

async function seedArtifactReports(
  groups: DailyRunGroup[],
  existingDays: Set<string>,
  options: BackfillOptions,
): Promise<Set<string>> {
  const seededDays = new Set<string>();
  if (options.historyOnly) return seededDays;

  log("phase 1/2: seeding still-retained GitHub artifact reports");
  const pending = groups.filter((group) => !existingDays.has(group.day));
  for (const [index, group] of pending.entries()) {
    log(
      `${group.day}: checking retained artifacts (${index + 1}/${pending.length}, ${group.runs.length} run(s) that day)`,
    );
    const candidate = await findDailyArtifactCandidate(group);
    if (!candidate) {
      log(
        `${group.day}: no retained artifact found; historical rerun may be needed`,
      );
      continue;
    }
    log(
      `${group.day}: selected run #${candidate.run.run_number ?? candidate.run.id}, artifact ${candidate.artifact.id}`,
    );
    seededDays.add(group.day);
    if (options.dryRun) continue;

    const reportJson = await downloadReportJson(candidate.artifact.id);
    const normalized = normalizeAnyTest262Report(JSON.parse(reportJson));
    if (!normalized) {
      log(
        `${group.day}: artifact ${candidate.artifact.id} did not contain a valid report`,
      );
      seededDays.delete(group.day);
      continue;
    }
    await publishEntry(
      {
        point: pointFromCandidate(candidate, normalized.report),
        report: normalized.report,
        reportJson: normalized.reportJson,
      },
      `${group.day}: artifact ${candidate.artifact.id}`,
    );
  }
  return seededDays;
}

async function runCommand(
  cmd: string,
  args: string[],
  options: { cwd: string; allowFailure?: boolean },
): Promise<number> {
  log(`$ ${[cmd, ...args].join(" ")} (${options.cwd})`);
  const proc = Bun.spawn([cmd, ...args], {
    cwd: options.cwd,
    env: { ...process.env, CI: "1" },
    stdout: "inherit",
    stderr: "inherit",
  });
  const exitCode = await proc.exited;
  if (exitCode !== 0 && !options.allowFailure) {
    throw new Error(`${cmd} ${args.join(" ")} exited with ${exitCode}`);
  }
  return exitCode;
}

async function pathExists(value: string): Promise<boolean> {
  try {
    await access(value);
    return true;
  } catch {
    return false;
  }
}

async function readSuitePin(
  worktree: string,
): Promise<{ pin: string; source: string }> {
  const historicalPin = path.join(worktree, "scripts", "test262-suite-sha.txt");
  const currentPin = path.join(repoRoot, "scripts", "test262-suite-sha.txt");
  const source = (await pathExists(historicalPin)) ? historicalPin : currentPin;
  const pin = (await readFile(source, "utf8")).trim();
  if (!/^[0-9a-f]{40}$/.test(pin)) {
    throw new Error(`invalid test262 suite pin in ${source}: ${pin}`);
  }
  return {
    pin,
    source:
      source === historicalPin
        ? "historical commit"
        : "current checkout fallback",
  };
}

async function ensureTest262Suite(pin: string, cacheRoot: string) {
  const suiteDir = path.join(cacheRoot, "test262", pin);
  if (await pathExists(path.join(suiteDir, "harness"))) return suiteDir;

  await mkdir(path.dirname(suiteDir), { recursive: true });
  await runCommand(
    "git",
    [
      "clone",
      "--filter=blob:none",
      "--no-checkout",
      "https://github.com/tc39/test262.git",
      suiteDir,
    ],
    { cwd: repoRoot },
  );
  await runCommand("git", ["fetch", "--depth=1", "origin", pin], {
    cwd: suiteDir,
  });
  await runCommand("git", ["checkout", "--force", pin], { cwd: suiteDir });
  return suiteDir;
}

async function runHistoricalRunner(
  worktree: string,
  suiteDir: string,
  reportPath: string,
  options: BackfillOptions,
) {
  const tsRunner = path.join(worktree, "scripts", "run_test262_suite.ts");
  const pythonRunner = path.join(worktree, "scripts", "run_test262_suite.py");

  if (await pathExists(tsRunner)) {
    log("using TypeScript test262 runner from historical commit");
    await runCommand("./build.pas", ["--clean", "loaderbare"], {
      cwd: worktree,
    });
    return await runCommand(
      "bun",
      [
        "scripts/run_test262_suite.ts",
        "--suite-dir",
        suiteDir,
        "--mode=bytecode",
        `--jobs=${options.jobs}`,
        `--output=${reportPath}`,
      ],
      { cwd: worktree, allowFailure: true },
    );
  }

  if (await pathExists(pythonRunner)) {
    log("using legacy Python test262 runner from historical commit");
    await runCommand("./build.pas", ["--clean", "testrunner", "loader"], {
      cwd: worktree,
    });
    return await runCommand(
      "python3",
      [
        "scripts/run_test262_suite.py",
        "--suite-dir",
        suiteDir,
        "--mode=bytecode",
        `--jobs=${options.jobs}`,
        "--output",
        reportPath,
      ],
      { cwd: worktree, allowFailure: true },
    );
  }

  throw new Error(`${worktree} has no recognized test262 runner`);
}

async function backfillRun(
  run: GitHubWorkflowRun,
  tempRoot: string,
  options: BackfillOptions,
): Promise<Test262BlobPublishEntry> {
  if (typeof run.head_sha !== "string") {
    throw new Error(`run #${run.run_number ?? run.id} has no head SHA`);
  }

  const day = utcDayKey(run.created_at) ?? "unknown-day";
  const worktree = path.join(
    tempRoot,
    `goccia-${day}-${run.head_sha.slice(0, 8)}`,
  );
  log(`${day}: creating detached worktree for ${run.head_sha}`);
  await runCommand(
    "git",
    ["fetch", "--no-tags", "--depth=1", "origin", run.head_sha],
    {
      cwd: repoRoot,
    },
  );
  await runCommand(
    "git",
    ["worktree", "add", "--detach", worktree, run.head_sha],
    { cwd: repoRoot },
  );

  try {
    const { pin, source } = await readSuitePin(worktree);
    log(`${day}: using test262 suite ${pin} (${source})`);
    const suiteDir = await ensureTest262Suite(pin, tempRoot);
    const reportPath = path.join(tempRoot, `${day}-${run.id}.json`);
    const exitCode = await runHistoricalRunner(
      worktree,
      suiteDir,
      reportPath,
      options,
    );
    log(`${day}: test262 runner exited with ${exitCode}`);

    const reportJson = await readFile(reportPath, "utf8");
    const normalized = normalizeAnyTest262Report(JSON.parse(reportJson));
    if (!normalized)
      throw new Error(`${reportPath} is not a valid test262 report`);

    return {
      point: pointFromRun(run, normalized.report),
      report: normalized.report,
      reportJson: normalized.reportJson,
    };
  } finally {
    if (options.keepWorktree) {
      log(`${day}: keeping worktree ${worktree}`);
    } else {
      await runCommand("git", ["worktree", "remove", "--force", worktree], {
        cwd: repoRoot,
        allowFailure: true,
      });
      await rm(worktree, { recursive: true, force: true });
    }
  }
}

async function rerunHistoricalReports(
  groups: DailyRunGroup[],
  existingDays: Set<string>,
  options: BackfillOptions,
) {
  if (options.artifactsOnly) return;

  const missing = groups.filter((group) => !existingDays.has(group.day));
  log(`phase 2/2: rerunning ${missing.length} missing historical day(s)`);
  for (const group of missing) {
    const run = group.runs[0];
    log(
      `${group.day}: selected run #${run.run_number ?? run.id}, ${run.head_sha}`,
    );
  }
  if (options.dryRun || missing.length === 0) return;

  const tempRoot = await mkdtemp(
    path.join(os.tmpdir(), "goccia-test262-history-"),
  );
  log(`using temporary directory ${tempRoot}`);
  try {
    for (const group of missing) {
      const entry = await backfillRun(group.runs[0], tempRoot, options);
      await publishEntry(entry, `${group.day}: historical rerun`);
      existingDays.add(group.day);
    }
  } finally {
    if (options.keepWorktree) {
      log(`keeping temporary directory ${tempRoot}`);
    } else {
      await rm(tempRoot, { recursive: true, force: true });
    }
  }
}

function usage(): never {
  console.error(`Usage:
  bun run backfill-test262 [options]

Default behavior:
  1. Read completed main-branch CI runs since ${DEFAULT_SINCE}.
  2. Publish the latest retained GitHub artifact for each missing UTC day.
  3. Rerun historical commits for any days whose artifact has expired.

Options:
  --since=YYYY-MM-DD   First UTC day to include; default ${DEFAULT_SINCE}.
  --until=YYYY-MM-DD   Last UTC day to include; default today.
  --dry-run            List what would be seeded or rerun without publishing.
  --force              Ignore days already present in Blob.
  --jobs=N             test262 worker count for historical reruns; default ${DEFAULT_JOBS}.
  --keep-worktree      Leave temporary worktrees behind for debugging.

Environment:
  BLOB_READ_WRITE_TOKEN       Required unless --dry-run is used.
  GITHUB_ARTIFACT_TOKEN       Required for downloading retained artifact ZIPs.
  GITHUB_TOKEN                Also accepted for GitHub artifact access.
  TEST262_BACKFILL_PAGE_LIMIT Internal safety cap for workflow-run pages; default ${INTERNAL_PAGE_LIMIT}.
`);
  process.exit(0);
}

async function main() {
  const options = parseOptions(process.argv.slice(2));
  log(`date range: ${options.since} through ${options.until}`);

  if (!options.dryRun && !process.env.BLOB_READ_WRITE_TOKEN) {
    throw new Error("Set BLOB_READ_WRITE_TOKEN to publish Vercel Blob data");
  }

  const existingDays = await loadExistingDays(options);
  if (existingDays.size > 0) {
    log(`${existingDays.size} day(s) already present in Blob manifest`);
  }

  const runs = await fetchWorkflowRuns(options);
  const groups = groupRunsByDay(runs, options);
  log(`found ${groups.length} UTC day(s) with completed main runs`);

  const seededDays = await seedArtifactReports(groups, existingDays, options);
  for (const day of seededDays) existingDays.add(day);

  await rerunHistoricalReports(groups, existingDays, options);
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  console.error(`[backfill-test262] failed: ${message}`);
  process.exit(1);
});
