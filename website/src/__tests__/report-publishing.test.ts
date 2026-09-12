import { afterAll, describe, expect, test } from "bun:test";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { timestampFromEnv } from "../../scripts/lib/report-publishing";

const directory = mkdtempSync(join(tmpdir(), "report-publishing-"));
const website = resolve(import.meta.dir, "../..");
const report = join(directory, "report.json");
const preload = join(directory, "blob-mock.ts");
const uploads = join(directory, "uploads.jsonl");
writeFileSync(
  report,
  JSON.stringify({ targets: [], results: [], summary: {} }),
);
writeFileSync(
  preload,
  `
import { mock } from "bun:test";
import { appendFileSync } from "node:fs";
mock.module(${JSON.stringify(import.meta.resolve("@vercel/blob"))}, () => ({
  BlobNotFoundError: class extends Error {},
  get: async () => null,
  list: async () => ({ blobs: [], hasMore: false }),
  put: async (pathname, body, options) => {
    appendFileSync(process.env.PUBLISH_TEST_OUTPUT, JSON.stringify({
      pathname, options, body: typeof body === "string" ? JSON.parse(body) : null,
    }) + "\\n");
    return { url: "https://blob.test/" + pathname, downloadUrl: "https://blob.test/" + pathname };
  },
}));
`,
);
afterAll(() => rmSync(directory, { recursive: true, force: true }));

function publish(
  script: string,
  overrides: Record<string, string>,
  args = [report],
) {
  const env = { ...process.env };
  for (const key of Object.keys(env)) {
    if (
      /^(GITHUB_|AWFY_|JETSTREAM_|WEB_TOOLING_|BENCHMARK_PROFILE_|TEST262_)/.test(
        key,
      )
    )
      delete env[key];
  }
  writeFileSync(uploads, "");
  const result = Bun.spawnSync(
    [
      process.execPath,
      "--preload",
      preload,
      `scripts/publish-${script}.ts`,
      ...args,
    ],
    {
      cwd: website,
      env: {
        ...env,
        BLOB_READ_WRITE_TOKEN: "test-only",
        PUBLISH_TEST_OUTPUT: uploads,
        ...overrides,
      },
    },
  );
  expect(result.exitCode, result.stderr.toString()).toBe(0);
  return readFileSync(uploads, "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line));
}

test("Unix timestamps respect the inclusive Date range and return null beyond it", () => {
  const name = "REPORT_PUBLISHING_TEST_TIMESTAMP";
  const previous = process.env[name];
  try {
    process.env[name] = "8640000000000";
    expect(timestampFromEnv(name)).toBe("+275760-09-13T00:00:00.000Z");
    for (const value of ["8640000000001", String(Number.MAX_SAFE_INTEGER)]) {
      process.env[name] = value;
      expect(timestampFromEnv(name)).toBeNull();
    }
  } finally {
    if (previous === undefined) delete process.env[name];
    else process.env[name] = previous;
  }
});

describe("report publisher commands", () => {
  test("preserves GitHub metadata, namespace overrides, and daily paths across publishers", () => {
    for (const [script, envPrefix, dailyPath] of [
      ["awfy-report", "AWFY", "custom/daily/2026-09-04.json"],
      ["jetstream-report", "JETSTREAM", "custom/daily/2026-09-04/101.json"],
      ["web-tooling-report", "WEB_TOOLING", "custom/daily/2026-09-04.json"],
      ["test262-results", "TEST262", "custom/daily/2026-09-04.json"],
      [
        "benchmark-profile-reports",
        "BENCHMARK_PROFILE",
        "custom/daily/2026-09-04.json",
      ],
      [
        "test262-profile-reports",
        "TEST262_PROFILE",
        "custom/daily/2026-09-04.json",
      ],
    ]) {
      const profile = script.includes("profile");
      const calls = publish(
        script,
        {
          GITHUB_RUN_ID: "101",
          GITHUB_RUN_NUMBER: "7",
          GITHUB_SHA: "abcdef123456",
          GITHUB_WORKFLOW: "Publish",
          GITHUB_SERVER_URL: "https://forge.test",
          GITHUB_REPOSITORY: "owner/repo",
          [`${envPrefix}_ARTIFACT_ID`]: "202",
          [`${envPrefix}_RUN_CREATED_AT`]: "2026-09-04T01:00:00Z",
          [`${envPrefix}_BLOB_PREFIX`]: " /custom/ ",
          [`${envPrefix === "TEST262_PROFILE" ? "TEST262" : envPrefix}_BLOB_ACCESS`]:
            "private",
        },
        profile ? ["--aggregate", report] : [report],
      );
      expect(calls.map((call) => call.pathname)).toEqual([
        profile
          ? "custom/runs/202/aggregate.json.gz"
          : script === "test262-results"
            ? "custom/runs/202.json.gz"
            : "custom/runs/202/report.json.gz",
        dailyPath,
      ]);
      expect(calls.every((call) => call.options.access === "private")).toBe(
        true,
      );
      expect(calls[1].body).toMatchObject({
        runId: 101,
        runNumber: 7,
        artifactId: 202,
        title: "Publish",
        headSha: "abcdef123456",
        shortSha: "abcdef12",
        runUrl: "https://forge.test/owner/repo/actions/runs/101",
        createdAt: "2026-09-04T01:00:00.000Z",
      });
      if (profile)
        expect(Object.keys(calls[1].body.profileReports)).toEqual([
          "aggregate",
        ]);
    }
  });

  test("preserves local defaults and test262 profile timestamp fallback", () => {
    const calls = publish(
      "test262-profile-reports",
      {
        TEST262_PROFILE_ARTIFACT_ID: "invalid",
        GITHUB_RUN_NUMBER: "1.5",
        TEST262_PROFILE_RUN_CREATED_AT: "8640000000001",
        TEST262_RUN_CREATED_AT: "1788483600",
        TEST262_PROFILE_BLOB_PREFIX: " /// ",
        TEST262_BLOB_ACCESS: "PRIVATE",
      },
      [`--aggregate=${report}`],
    );
    const pointer = calls[1].body;
    expect(pointer.runId).toBe(pointer.artifactId);
    expect(Number.isSafeInteger(pointer.artifactId)).toBe(true);
    expect(pointer).toMatchObject({
      runNumber: 0,
      title: "CI",
      headSha: "unknown",
      runUrl: "https://github.com/frostney/GocciaScript",
      createdAt: "2026-09-04T01:00:00.000Z",
    });
    expect(calls[1].pathname).toBe("test262-profiles/daily/2026-09-04.json");
    expect(calls[0].options.access).toBe("public");
  });

  test("keeps JetStream's date-only timestamp parser and artifact fallback", () => {
    const calls = publish("jetstream-report", {
      GITHUB_RUN_ID: "101",
      JETSTREAM_ARTIFACT_ID: "0",
      JETSTREAM_RUN_CREATED_AT: "1788483600",
    });
    expect(calls[1].body.artifactId).toBe(101);
    expect(calls[1].body.createdAt).toBe(calls[1].body.updatedAt);
  });

  test("reads optional profile attachments with both CLI argument forms", () => {
    const markdown = join(directory, "summary.md");
    const archive = join(directory, "details.tar.gz");
    writeFileSync(markdown, "# profile\n");
    writeFileSync(archive, Buffer.from([0, 255, 1]));
    for (const script of [
      "benchmark-profile-reports",
      "test262-profile-reports",
    ]) {
      const calls = publish(script, {}, [
        `--aggregate=${report}`,
        "--markdown",
        markdown,
        `--details-archive=${archive}`,
      ]);
      expect(calls).toHaveLength(4);
      expect(calls[3].body.profileReports.markdown.size).toBe(10);
      expect(calls[3].body.profileReports.detailsArchive.size).toBe(3);
      expect(calls[1].options.contentType).toBe("text/markdown; charset=utf-8");
    }
  });
});
