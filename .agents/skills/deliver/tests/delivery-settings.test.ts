import { afterEach, expect, test } from "bun:test";
import { spawnSync } from "node:child_process";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const script = fileURLToPath(new URL("../scripts/delivery-settings.mjs", import.meta.url));
const projects: string[] = [];

afterEach(async () => {
  await Promise.all(projects.splice(0).map((path) => rm(path, { force: true, recursive: true })));
});

async function project(orchestration?: string) {
  const root = await mkdtemp(join(tmpdir(), "kgr-delivery-settings-"));
  projects.push(root);
  if (orchestration !== undefined) await writeFile(join(root, "ORCHESTRATION.md"), orchestration);
  return root;
}

function read(root: string) {
  const run = spawnSync("node", [script, root], { encoding: "utf8" });
  return { status: run.status, stderr: run.stderr, settings: run.status === 0 ? JSON.parse(run.stdout) : null };
}

test("a project without ORCHESTRATION.md continues entry points to ready-to-merge", async () => {
  expect(read(await project())).toEqual({
    status: 0,
    stderr: "",
    settings: { endpoint: "ready-to-merge", "entry-points": "deliver", file: null, defaulted: ["endpoint", "entry-points"] },
  });
});

test("a key the file leaves out keeps its default, and prose without frontmatter sets nothing", async () => {
  expect(read(await project("---\nendpoint: 'deployed' # nightly\nowner: platform\n---\n\n# Policy\n")).settings).toEqual({
    endpoint: "deployed",
    "entry-points": "deliver",
    file: "ORCHESTRATION.md",
    defaulted: ["entry-points"],
  });
  expect(read(await project("# Policy\n\nendpoint: merged\n")).settings).toMatchObject({
    endpoint: "ready-to-merge",
    defaulted: ["endpoint", "entry-points"],
  });
});

test("both keys are read from CRLF frontmatter", async () => {
  expect(read(await project('---\r\nendpoint: merged\r\nentry-points: "stop"\r\n---\r\n')).settings).toMatchObject({
    endpoint: "merged",
    "entry-points": "stop",
    defaulted: [],
  });
});

test("an unknown value or unterminated frontmatter fails instead of guessing", async () => {
  const unknown = read(await project("---\nendpoint: production\n---\n"));
  expect(unknown.status).toBe(1);
  expect(unknown.stderr).toContain('sets endpoint to "production"');
  const unterminated = read(await project("---\nentry-points: stop\n"));
  expect(unterminated.status).toBe(1);
  expect(unterminated.stderr).toContain("no closing ---");
});
