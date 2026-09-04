import { readFile } from "node:fs/promises";
import { GITHUB_REPO_URL } from "../../src/lib/github";

function numberFromEnv(name: string): number | null {
  const value = process.env[name];
  if (!value) return null;
  const parsed = Number(value);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
}

export function timestampFromEnv(
  name: string,
  unixSeconds = true,
): string | null {
  const value = process.env[name]?.trim();
  if (!value) return null;
  if (unixSeconds && /^\d+$/.test(value)) {
    const seconds = Number(value);
    if (Number.isSafeInteger(seconds) && seconds > 0) {
      const date = new Date(seconds * 1000);
      return Number.isNaN(date.getTime()) ? null : date.toISOString();
    }
  }
  const time = Date.parse(value);
  return Number.isNaN(time) ? null : new Date(time).toISOString();
}

export function currentRunMetadata(
  artifactIdEnv: string,
  createdAt: string | null,
) {
  const runId = numberFromEnv("GITHUB_RUN_ID");
  const artifactId = numberFromEnv(artifactIdEnv) ?? runId ?? Date.now();
  const repository = process.env.GITHUB_REPOSITORY ?? "frostney/GocciaScript";
  const server = process.env.GITHUB_SERVER_URL ?? "https://github.com";
  const headSha = process.env.GITHUB_SHA ?? "unknown";
  const now = new Date().toISOString();
  return {
    runId: runId ?? artifactId,
    runNumber: numberFromEnv("GITHUB_RUN_NUMBER") ?? 0,
    artifactId,
    title: process.env.GITHUB_WORKFLOW ?? "CI",
    headSha,
    shortSha: headSha.slice(0, 8),
    runUrl: runId
      ? `${server}/${repository}/actions/runs/${runId}`
      : GITHUB_REPO_URL,
    createdAt: createdAt ?? now,
    updatedAt: now,
    artifactCreatedAt: now,
  };
}

export function parseProfileArgs(
  args: string[],
  usage: () => never,
): {
  aggregate: string;
  markdown: string | null;
  detailsArchive: string | null;
} {
  let aggregate: string | null = null;
  let markdown: string | null = null;
  let detailsArchive: string | null = null;

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--aggregate") aggregate = args[++i] ?? null;
    else if (arg.startsWith("--aggregate=")) {
      aggregate = arg.slice("--aggregate=".length);
    } else if (arg === "--markdown") markdown = args[++i] ?? null;
    else if (arg.startsWith("--markdown=")) {
      markdown = arg.slice("--markdown=".length);
    } else if (arg === "--details-archive") detailsArchive = args[++i] ?? null;
    else if (arg.startsWith("--details-archive=")) {
      detailsArchive = arg.slice("--details-archive=".length);
    } else if (arg === "--help" || arg === "-h") {
      usage();
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }

  if (!aggregate) usage();
  return { aggregate, markdown, detailsArchive };
}

async function readJsonFile(filePath: string): Promise<string> {
  const raw = await readFile(filePath, "utf8");
  const parsed = JSON.parse(raw) as unknown;
  return `${JSON.stringify(parsed, null, 2)}\n`;
}

export async function readProfileFiles(
  aggregatePath: string,
  markdownPath: string | null,
  detailsArchivePath: string | null,
) {
  return {
    aggregateJson: await readJsonFile(aggregatePath),
    markdown: markdownPath ? await readFile(markdownPath) : undefined,
    detailsArchive: detailsArchivePath
      ? await readFile(detailsArchivePath)
      : undefined,
  };
}
