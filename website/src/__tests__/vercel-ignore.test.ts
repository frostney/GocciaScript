import { afterAll, beforeAll, describe, expect, test } from "bun:test";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";

/** `scripts/vercel-ignore.sh` decides whether Vercel builds at all, so its
 *  failure modes are invisible until the site is already stale. These run the
 *  real script against a throwaway git repository, with the two network
 *  lookups pointed at `file://` fixtures. */

const SCRIPT = path.join(
  import.meta.dir,
  "..",
  "..",
  "scripts",
  "vercel-ignore.sh",
);

let repo: string;
let fixtures: string;

function fixtureUrl(name: string): string {
  return `file://${path.join(fixtures, name)}`;
}

function writeFixture(name: string, body: string): string {
  writeFileSync(path.join(fixtures, name), body);
  return fixtureUrl(name);
}

function git(...args: string[]): void {
  const result = spawnSync("git", args, { cwd: repo, encoding: "utf8" });
  if (result.status !== 0) {
    throw new Error(`git ${args.join(" ")} failed: ${result.stderr}`);
  }
}

function commit(file: string, contents: string): string {
  const target = path.join(repo, file);
  mkdirSync(path.dirname(target), { recursive: true });
  writeFileSync(target, contents);
  git("add", "-A");
  git(
    "-c",
    "user.email=t@example.com",
    "-c",
    "user.name=T",
    "commit",
    "-m",
    file,
  );
  return spawnSync("git", ["rev-parse", "HEAD"], {
    cwd: repo,
    encoding: "utf8",
  }).stdout.trim();
}

/** Exit code of the ignore step: 0 = skip the build, 1 = build. */
function runIgnore(env: Record<string, string>): {
  code: number;
  output: string;
} {
  const result = spawnSync("bash", [SCRIPT], {
    cwd: repo,
    encoding: "utf8",
    env: {
      // Deliberately minimal: the script must not depend on anything Vercel
      // does not set.
      NODE_ENV: process.env.NODE_ENV ?? "test",
      PATH: process.env.PATH ?? "",
      HOME: process.env.HOME ?? "",
      ...env,
    },
  });
  return {
    code: result.status ?? -1,
    output: `${result.stdout}${result.stderr}`,
  };
}

let baseSha: string;
let websiteChangeSha: string;
let releaseOnlySha: string;

beforeAll(() => {
  repo = mkdtempSync(path.join(tmpdir(), "goccia-ignore-repo-"));
  fixtures = mkdtempSync(path.join(tmpdir(), "goccia-ignore-fixtures-"));
  git("init", "-q", "-b", "main");
  mkdirSync(path.join(repo, "website"), { recursive: true });
  writeFileSync(path.join(repo, "website", "package.json"), "{}\n");
  baseSha = commit("README.md", "start\n");
  websiteChangeSha = commit("website/src/app/page.tsx", "export default 1;\n");
  // A release commit as this repo actually produces one: CHANGELOG.md only.
  releaseOnlySha = commit("CHANGELOG.md", "## 0.11.0\n");
});

afterAll(() => {
  rmSync(repo, { recursive: true, force: true });
  rmSync(fixtures, { recursive: true, force: true });
});

describe("website change detection", () => {
  test("builds when website/ changed", () => {
    const { code, output } = runIgnore({
      VERCEL_GIT_PREVIOUS_SHA: baseSha,
      VERCEL_GIT_COMMIT_SHA: websiteChangeSha,
      GOCCIA_SKIP_RELEASE_CHECK: "1",
    });
    expect(code).toBe(1);
    expect(output).toContain("Website changes detected");
  });

  test("skips a commit that touches nothing under website/", () => {
    const { code } = runIgnore({
      VERCEL_GIT_PREVIOUS_SHA: websiteChangeSha,
      VERCEL_GIT_COMMIT_SHA: releaseOnlySha,
      GOCCIA_SKIP_RELEASE_CHECK: "1",
    });
    expect(code).toBe(0);
  });
});

describe("release freshness", () => {
  const siteEnv = (latest: string, site: string) => ({
    VERCEL_GIT_PREVIOUS_SHA: websiteChangeSha,
    VERCEL_GIT_COMMIT_SHA: releaseOnlySha,
    GOCCIA_LATEST_RELEASE_URL: latest,
    GOCCIA_SITE_VERSIONS_URL: site,
  });

  test("builds when the live site has not vendored the newest release", () => {
    // The 0.11.0 regression: release lands, nothing under website/ changes,
    // the site keeps serving 0.10.0 + nightly forever.
    const latest = writeFixture("latest-0110.json", '{"tag_name": "0.11.0"}');
    const site = writeFixture(
      "site-stale.json",
      '{"defaultVersion":"0.10.0","vendored":["0.10.0","0.9.0","nightly"],"playground":["nightly"]}',
    );
    const { code, output } = runIgnore(siteEnv(latest, site));
    expect(code).toBe(1);
    expect(output).toContain("Release 0.11.0 is not vendored");
  });

  test("skips once the live site vendors that release (so the redeploy settles)", () => {
    const latest = writeFixture("latest-0110b.json", '{"tag_name": "0.11.0"}');
    const site = writeFixture(
      "site-fresh.json",
      '{"defaultVersion":"0.11.0","vendored":["0.11.0","0.10.0","nightly"],"playground":["0.11.0","nightly"]}',
    );
    const { code, output } = runIgnore(siteEnv(latest, site));
    expect(code).toBe(0);
    expect(output).toContain("already vendors release 0.11.0");
  });

  test("treats a `v`-prefixed release tag as the same version", () => {
    const latest = writeFixture("latest-v.json", '{"tag_name": "v0.11.0"}');
    const site = writeFixture(
      "site-fresh-v.json",
      '{"defaultVersion":"0.11.0","vendored":["0.11.0","nightly"]}',
    );
    expect(runIgnore(siteEnv(latest, site)).code).toBe(0);
  });

  test("ignores prerelease-only vendored tags when matching", () => {
    // A site serving only `nightly` must never look current against a
    // published stable.
    const latest = writeFixture("latest-strict.json", '{"tag_name": "0.11.0"}');
    const site = writeFixture(
      "site-nightly-only.json",
      '{"defaultVersion":"nightly","vendored":["nightly"],"playground":["nightly"]}',
    );
    expect(runIgnore(siteEnv(latest, site)).code).toBe(1);
  });

  test("builds when the release lookup fails", () => {
    const site = writeFixture("site-any.json", '{"vendored":["0.11.0"]}');
    const { code, output } = runIgnore(
      siteEnv(fixtureUrl("missing-release.json"), site),
    );
    expect(code).toBe(1);
    expect(output).toContain("Could not read the newest stable release");
  });

  test("builds when the deployed site cannot be read", () => {
    const latest = writeFixture("latest-any.json", '{"tag_name": "0.11.0"}');
    const { code, output } = runIgnore(
      siteEnv(latest, fixtureUrl("missing-site.json")),
    );
    expect(code).toBe(1);
    expect(output).toContain("Could not read vendored versions");
  });
});
